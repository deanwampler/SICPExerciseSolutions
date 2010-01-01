// Set representation with no duplicate entries in the list.

def elementOfSet[T] (t:T, set: List[T]): Boolean = set match {
  case Nil => false
  case head::tail => head match {
    case _ if (head == t) => true
    case _ => elementOfSet (t, tail)
  }
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

def adjoinSet[T] (t:T, set: List[T]) = 
  if (elementOfSet (t, set)) 
    set 
  else 
    t :: set

object adjoinSetSpec extends Spec with ShouldMatchers {
  describe ("adjoinSet") {
    it ("should return a new set with new element as the head and the old set as the tail") {
      adjoinSet (1, Nil) should equal (List(1))
      adjoinSet (1, List(2)) should equal (List(1,2))
      adjoinSet (1, List(2,3,4)) should equal (List(1,2,3,4))
    }
  }
}
adjoinSetSpec execute

def intersectionSet[T] (set1: List[T], set2: List[T]): List[T] = {
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

def unionSet[T] (set1: List[T], set2: List[T]): List[T] = {
  if      (set1 == Nil) set2
  else if (set2 == Nil) set1
  else if (elementOfSet (set1.head, set2))
    unionSet (set1.tail, set2)
  else 
    set1.head :: unionSet (set1.tail, set2)
}

object unionSetSpec extends Spec with ShouldMatchers {
  describe ("unionSet") {
    it ("should return the union of two sets") {
      unionSet (Nil, Nil) should equal (Nil)
      unionSet (Nil, List(1,2,3)) should equal (List(1,2,3))
      unionSet (List(1,2,3), Nil) should equal (List(1,2,3))
      unionSet (List(2), List(2,3,4,5)) should equal (List(2,3,4,5))
      // unionSet doesn't sort.
      unionSet (List(2,3,4,5), List(2)) should equal (List(3,4,5,2))
      unionSet (List(2,3,4,5), List(1,2,3)) should equal (List(4,5,1,2,3))
      unionSet (List(1,2,3), List(2,3,4,5)) should equal (List(1,2,3,4,5))
    }
  }
}
unionSetSpec execute
