// Set representation with duplicate entries in the list.

// Unchanged, but it will be slower due to the redundant entries.
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

// Now just cons the element to the set, rather than checking first if x is already
// in the set. This turns the function from O(n) to O(1).
def adjoinSet[T] (t:T, set: List[T]) = t :: set

object adjoinSetSpec extends Spec with ShouldMatchers {
  describe ("adjoinSet") {
    it ("should return a new set with new element as the head and the old set as the tail") {
      adjoinSet (1, Nil) should equal (List(1))
      adjoinSet (1, List(1)) should equal (List(1,1))
      adjoinSet (1, List(2)) should equal (List(1,2))
      adjoinSet (1, List(1,2)) should equal (List(1,1,2))
      adjoinSet (1, List(2,3,4)) should equal (List(1,2,3,4))
    }
  }
}
adjoinSetSpec execute

// Must use the same implementation to find the true intersection. Will be slower
// due to the redundant entries, but the resulting set will have no duplicates.
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

// Now just joins the two sets, rather than checking first for duplicates.
// This turns the function from O(n) to O(1).
def unionSet[T] (set1: List[T], set2: List[T]): List[T] = set1 ++ set2

object unionSetSpec extends Spec with ShouldMatchers {
  describe ("unionSet") {
    it ("should return the union of two sets") {
      unionSet (Nil, Nil) should equal (Nil)
      unionSet (Nil, List(1,2,3)) should equal (List(1,2,3))
      unionSet (List(1,2,3), Nil) should equal (List(1,2,3))
      unionSet (List(2), List(2,3,4,5)) should equal (List(2,2,3,4,5))
      unionSet (List(2,3,4,5), List(2)) should equal (List(2,3,4,5,2))
      unionSet (List(2,3,4,5), List(1,2,3)) should equal (List(2,3,4,5,1,2,3))
      unionSet (List(1,2,3), List(2,3,4,5)) should equal (List(1,2,3,2,3,4,5))
    }
  }
}
unionSetSpec execute

// Since the representations of a set are no longer unique, e.g., 
// List(1, 1, 1, 3) represents the same set as List(1, 2), we really need an
// equality method.
def equalSets[T] (set1: List[T], set2: List[T]) = {
  def checkOne (s1: List[T], s2: List[T]): Boolean =
    if      (s1 == Nil) true
    else if (elementOfSet (s1.head, s2))
      checkOne (s1.tail, s2)
    else false
  checkOne (set1, set2) && checkOne (set2, set1)
}

object equalSetsSpec extends Spec with ShouldMatchers {
  describe ("equalSets") {
    it ("should return true if the sets are logically equal") {
      equalSets (Nil, Nil) should equal (true)
      equalSets (Nil, List(1)) should equal (false)
      equalSets (List(1), Nil) should equal (false)
      equalSets (List(1), List(1)) should equal (true)
      equalSets (List(1,1), List(1)) should equal (true)
      equalSets (List(1), List(1,1)) should equal (true)
      equalSets (List(1,1,1), List(1)) should equal (true)
      equalSets (List(1), List(1,1,1)) should equal (true)
      equalSets (List(1,2,3,4,5), List(1,2,3,4,5)) should equal (true)
      equalSets (List(1,2,3,4,5), List(1,2,1,2,3,3,4,4,5,4,5,3,1,2,3)) should equal (true)
      equalSets (List(1,2,1,2,3,3,4,4,5,4,5,3,1,2,3), List(1,2,3,4,5)) should equal (true)
    }
  }
}
equalSetsSpec execute

// This implementation would be best when adjoin performance is more important 
// than the performance of all the other operations. That is, when you want to 
// add to the set in O(1) time, rather than O(n) time, and you're willing to accept
// performance of the other methods that are still O(n) or O(n*n), but with a higher
// constant value.
