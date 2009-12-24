
def equalList (l1: List[_],  l2: List[_]): Boolean = {
  if      (l1 == Nil && l2 == Nil) true
  else if (l1 == Nil || l2 == Nil) false
  else if (l1.head == l2.head) equalList(l1.tail, l2.tail)
  else false
}

import org.scalatest._ 
import org.scalatest.matchers._

object equalListSpec extends Spec with ShouldMatchers {
  describe ("equalList") {
    it ("should return true if two lists are equal") {
      equalList(List("this", "is", "a", "list"), List("this", "is", "a", "list")) should equal (true)
      equalList(List("this", "is", "a", "list"), List(this, List("is", "a"), "list")) should equal (false)
      equalList(List("this", "is", "a"), List("this", "is", "a", "list")) should equal (false)
      equalList(List("this", "is", "a", "list"), List("this", "is", "a")) should equal (false)
      equalList(List("this", "is", "A", "list"), List("this", "is", "a", "list")) should equal (false)
      equalList(Nil, List("this", "is", "a", "list")) should equal (false)
      equalList(List("this", "is", "a", "list"), Nil) should equal (false)
      equalList(Nil, Nil) should equal (true)
    }
  }
}
equalListSpec execute
