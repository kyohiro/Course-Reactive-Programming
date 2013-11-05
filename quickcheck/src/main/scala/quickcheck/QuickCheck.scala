package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  //adding a to empty heap get min a
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("test meld/insert/findMin/deleteMin") = forAll { (h: H, h2: H, a: A) =>
    val h1 = insert(a, h) //make h1 a non empty heap
    val h3 = meld(h1, h2)
    val m = findMin(h1)
    val h4 = meld(deleteMin(h1), insert(m, h2))
    compareHeaps(h3, h4) == true
  }

  //compare if two heaps are exactly the same
  def compareHeaps(h1: H, h2: H): Boolean = {
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1) || isEmpty(h2)) false
    else if (findMin(h1) != findMin(h2)) false
    else compareHeaps(deleteMin(h1), deleteMin(h2)) 
  }
  
  def compareHeapWithMin(h: H, m: A): Boolean = {
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
      if (min < m) false
      else compareHeapWithMin(deleteMin(h), min) 
    }
  }
  
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    m <- oneOf(genHeap, value(empty))
  } yield insert(a, m) 

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
