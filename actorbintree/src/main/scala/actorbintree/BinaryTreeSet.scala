/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import scala.concurrent.Promise

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply
  
}


class BinaryTreeSet extends Actor with Stash{
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  def receive = normal
  
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case operation: Operation => root ! operation
    case GC => val newRoot = createRoot
               root ! CopyTo(newRoot)
               context.become(garbageCollecting(newRoot))
  }
     
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case message: Operation => stash() //Leverage akka stash...
    case CopyFinished => {
      root ! PoisonPill
      root = newRoot
      context.become(normal)
      unstashAll()
    }
  }
}

object BinaryTreeNode {
  trait Position

  case object Middle extends Position
  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  /** See which branch is the element in **/
  def branch(elem: Int): Position = if (this.elem == elem) Middle else if (this.elem < elem) Right else Left 
 
  /** Go through branches to the subtree the element should be in **/
  def findBranch(op: Operation)(lastTree: Position => Unit) {
    branch(op.elem) match {
      case pos @ (Left | Right) if (hasSubNode(pos)) => subtrees(pos) ! op  //When next branch is available
      case pos => lastTree(pos)                                             //When in the middle or next branch is not available, apply operation 
    }
  }
  
  def hasSubNode(sub: Position) = subtrees.contains(sub)
  
  def receive = normal

  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case op @ Insert(requester, id, elem) => findBranch(op) {
      pos => pos match {
        case Middle => removed = false
        case _ => subtrees += pos -> context.actorOf(BinaryTreeNode.props(elem, initiallyRemoved = false))
      }
      requester ! OperationFinished(id)  
    } 
    case op @ Contains(requester, id, elem) => findBranch(op) {
      case Middle => requester ! ContainsResult(id, !removed)
      case _ => requester ! ContainsResult(id, false)
    } 
    case op @ Remove(requester, id, elem) => findBranch(op) { 
      pos => {
        if (pos == Middle) removed = true
        requester ! OperationFinished(id)
      }
    }
    case CopyTo(newRoot) => {
      if (!removed) newRoot ! Insert(self, elem, elem) //Use elem for id, as it's supposed to be distinct
      subtrees.values.foreach(_ ! CopyTo(newRoot))
      context.become(copying(subtrees.values.toSet, insertConfirmed=removed))
    }
  }

  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    if (expected.isEmpty && insertConfirmed) {
      context.parent ! CopyFinished
      normal
    }
    else {
      case OperationFinished(id) => if (id == elem) context.become(copying(expected, insertConfirmed = true))
      case CopyFinished => context.become(copying(expected - sender, insertConfirmed))
    }
  } 
}
