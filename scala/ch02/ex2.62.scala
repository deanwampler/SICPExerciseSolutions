// union-set for set representation with ordered entries (assume numbers).

def elementOfSet (i:Int, set: List[Int]): Boolean = set match {
  case Nil => false
  case head::tail => 
    if      (head == i) true
    else if (head >  i) false
    else elementOfSet (i, tail)
}

def unionSet (set1: List[Int], set2: List[Int]): List[Int] = {
  if      (set1 == Nil) set2
  else if (set2 == Nil) set1
  else { 
    val i1 = set1.head
    val i2 = set2.head
    if      (i1 == i2)
      i1 :: unionSet (set1.tail, set2.tail)
    else if (i1 < i2)
      i1 :: unionSet (set1.tail, set2)
    else if (i1 < i2)
      i1 :: unionSet (set1.tail, set2)
    else
      i2 :: unionSet (set1, set2.tail)
  }
}

import org.scalatest._ 
import org.scalatest.matchers._

object unionSetSpec extends Spec with ShouldMatchers {
  describe ("unionSet") {
    it ("should return the union of two sets") {
      unionSet (Nil, Nil) should equal (Nil)
      unionSet (Nil, List(1,2,3)) should equal (List(1,2,3))
      unionSet (List(1,2,3), Nil) should equal (List(1,2,3))
      unionSet (List(2), List(2,3,4,5)) should equal (List(2,3,4,5))
      unionSet (List(3), List(2,3,4,5)) should equal (List(2,3,4,5))
      unionSet (List(4), List(2,3,4,5)) should equal (List(2,3,4,5))
      unionSet (List(5), List(2,3,4,5)) should equal (List(2,3,4,5))
      unionSet (List(2,3,4,5), List(2)) should equal (List(2,3,4,5))
      unionSet (List(2,3,4,5), List(3)) should equal (List(2,3,4,5))
      unionSet (List(2,3,4,5), List(4)) should equal (List(2,3,4,5))
      unionSet (List(2,3,4,5), List(5)) should equal (List(2,3,4,5))
      unionSet (List(2,3,4,5), List(1,2,3)) should equal (List(1,2,3,4,5))
      unionSet (List(1,2,3), List(2,3,4,5)) should equal (List(1,2,3,4,5))
    }
  }
}
unionSetSpec execute
