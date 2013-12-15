package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.{Restart, Stop}
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import scala.language.postfixOps
import akka.dispatch.Foreach
import akka.actor.AllForOneStrategy
import akka.actor.ActorKilledException

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case object RetryPersist
  case class TimeOut(id: Long)
  
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  
  override def supervisorStrategy = AllForOneStrategy(){
    case _:PersistenceException => Restart
    case _:ActorKilledException => Stop 
  }
  
  context.system.scheduler.schedule(100 millis, 100 millis, context.self, RetryPersist)
  
  val beforeStart = arbiter ! Join
    
  val persistence = context.actorOf(persistenceProps)
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  //persisting list that hasn't been acked
  var persisting = Map.empty[Long, (ActorRef, Persist)]
  
  var expectedVersion = 0L
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }
  
  val leader: Receive = {
    case Insert(key, value, id) => {
      kv += key -> value
      doPersistPrimary(id, key, Some(value))
    }
    case Remove(key, id) => {
      kv -= key 
      doPersistPrimary(id, key, None)
    }
    case Get(key, id) => {
      sender ! GetResult(key, kv.get(key), id)
    }
    case RetryPersist => {
      persisting.foreach(entry => {
        val (_, (_, command)) = entry
        persistence ! command
      })
    }
    case Persisted(key, id) => {
      replicators.foreach(_  ! Replicate(key, kv.get(key), id))
      persisting.get(id).map(entry => {
        entry._1 ! OperationAck(id)
      })
      persisting -= id
    }
    case TimeOut(id) => {
      persisting.get(id).map(entry => {
        entry._1 ! OperationFailed(id) 
      })
      persisting -= id
    }
    case Replicas(replicas) => {
      var newReplicators = Set.empty[ActorRef]
      secondaries = Map.empty[ActorRef, ActorRef]
      replicas.foreach(replica => {
        if (replica != self) {
          val replicator = context.actorOf(Replicator.props(replica))
          newReplicators += replicator
          secondaries += replica -> replicator
          kv.foreach(entry => replicator ! Replicate(entry._1, Some(entry._2), nextSeq))
        }
      })
      replicators.foreach(context.stop)
      replicators = newReplicators
    }
  }
  
  val replica: Receive = {
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Snapshot(key, valueOpt, seq) => {
      if (seq == expectedVersion) {
        valueOpt match {
          case None => kv -= key
          case Some(value) => kv += key -> value
        }
        doPersist(seq, key, valueOpt) 
      } 
      else if (seq < expectedVersion) sender ! SnapshotAck(key, seq)
    }
    case RetryPersist => {
      persisting.foreach(entry => {
        val (_, (_, command)) = entry
        persistence ! command
      })
    }
    case Persisted(key, id) => {
      persisting.get(id).map(entry => {
        expectedVersion += 1
        entry._1 ! SnapshotAck(key, id)
      })
      persisting -= id
    }
  }
  
  def doPersist(id: Long, key: String, valueOpt: Option[String]) = {
    val command = Persist(key, valueOpt, id)
    persisting += id -> (sender, command) 
    persistence ! command
  }
  
  def doPersistPrimary(id: Long, key: String, valueOpt: Option[String]) = {
    doPersist(id, key, valueOpt)
    context.system.scheduler.scheduleOnce(1000 millis, context.self, TimeOut(id))
  }
}
