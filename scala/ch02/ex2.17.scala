class ListTooSmall extends RuntimeException

def lastPair[A](l: List[A]) = {
  if (l.size < 2) throw new ListTooSmall
  def lp(l2: List[A], pair: Tuple2[A,A]): Tuple2[A,A] = l2 match {
    case Nil => pair
    case _ => lp(l2.tail, (pair._2, l2.head))
  }
  lp(l.tail.tail, (l.head, l.tail.head))
}

import org.scalatest._ 
import org.scalatest.matchers._

object lastPairSpec extends Spec with ShouldMatchers {
  describe ("lastPair") {
    it ("should return a tuple with the last two list elements") {
      lastPair(List(1, 2, 3, 4, 5, 6)) should equal ((5, 6))
      lastPair(List(1, 2, 3)) should equal ((2, 3))
      lastPair(List(1, 2)) should equal ((1, 2))
    }
    it ("should throw an exception if the list has less than 2 elements") {
      intercept[ListTooSmall] {
        lastPair(List(1))
      }
      intercept[ListTooSmall] {
        lastPair(Nil)
      }
    }
    
  }
} 

lastPairSpec execute
