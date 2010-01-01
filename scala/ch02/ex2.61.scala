// Set representation with ordered entries in the list.

def elementOfSet (i:Int, set: List[Int]): Boolean = set match {
  case Nil => false
  case head::tail => 
    if      (head == i) true
    else if (head >  i) false
    else elementOfSet (i, tail)
}

import org.scalatest._ 
import org.scalatest.matchers._

object elementOfSetSpec extends Spec with ShouldMatchers {
  describe ("elementOfSet") {
    it ("should return false if the item is not in the set") {
      elementOfSet (1, Nil) should equal (false)
      elementOfSet (1, List(2,3,4)) should equal (false)
    }
    it ("should return true if the item is in the set") {
      elementOfSet (1, List(1,2,3,4)) should equal (true)
      elementOfSet (1, List(1)) should equal (true)
    }
  }
}
elementOfSetSpec execute

// This is also O(n), but on average takes 1/2 the time of adjoin-set in an
// unordered list implementation, just like element-of-set? above. This is true
// because the traversal of the existing set will stop when one of the two
// conditionals (= or <) is true.
def adjoinSet (i:Int, set: List[Int]): List[Int] = {
  if (set == Nil)
    List (i)
  else {
    val s = set.head
    if      (i == s) set
    else if (i < s) i :: set
    else s :: adjoinSet(i, set.tail)
  }
}
  
object adjoinSetSpec extends Spec with ShouldMatchers {
  describe ("adjoinSet") {
    it ("should return a new set with new element as the head and the old set as the tail") {
      adjoinSet (1, Nil) should equal (List(1))
      adjoinSet (1, List(1)) should equal (List(1))
      adjoinSet (1, List(2)) should equal (List(1,2))
      adjoinSet (1, List(1,2)) should equal (List(1,2))
      adjoinSet (1, List(1,2,3,4)) should equal (List(1,2,3,4))
      adjoinSet (2, List(1,2,3,4)) should equal (List(1,2,3,4))
      adjoinSet (3, List(1,2,3,4)) should equal (List(1,2,3,4))
      adjoinSet (4, List(1,2,3,4)) should equal (List(1,2,3,4))
      adjoinSet (1, List(2,3,4)) should equal (List(1,2,3,4))
      adjoinSet (2, List(1,3,4)) should equal (List(1,2,3,4))
      adjoinSet (3, List(1,2,4)) should equal (List(1,2,3,4))
      adjoinSet (4, List(1,2,3)) should equal (List(1,2,3,4))
    }
  }
}
adjoinSetSpec execute

def intersectionSet (set1: List[Int], set2: List[Int]): List[Int] = {
  if      (set1 == Nil || set2 == Nil) Nil
  else if (elementOfSet (set1.head, set2))
    set1.head :: intersectionSet (set1.tail, set2)
  else 
    intersectionSet (set1.tail, set2)
}

object intersectionSetSpec extends Spec with ShouldMatchers {
  describe ("intersectionSet") {
    it ("should return the intersection of two sets") {
      intersectionSet (Nil, Nil) should equal (Nil)
      intersectionSet (Nil, List(1,2,3)) should equal (Nil)
      intersectionSet (List(1,2,3), Nil) should equal (Nil)
      intersectionSet (List(2), List(2,3,4,5)) should equal (List(2))
      intersectionSet (List(2,3,4,5), List(2)) should equal (List(2))
      intersectionSet (List(2,3,4,5), List(1,2,3)) should equal (List(2,3))
      intersectionSet (List(1,2,3), List(2,3,4,5)) should equal (List(2,3))
    }
  }
}
intersectionSetSpec execute
