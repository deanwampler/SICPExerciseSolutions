def reverse[A](l: List[A]) = {
  def rev(l2: List[A], result: List[A]): List[A] = l2 match {
    case Nil => result
    case _ => rev(l2.tail, l2.head :: result)
  }
  rev(l, Nil)
}

// I attempted to use a type parameter (or more) instead of "_" in the lists, but
// it wouldn't type check when I called rev(head2::tail2, Nil) :: result, because
// it couldn't tell that the returned list is actually the correct type.
def deepReverse(l: List[_]): List[_] = {
  def rev(l2: List[_], result: List[_]): List[_] = l2 match {
    case Nil => result
    case head :: tail => head match {
      case head2 :: tail2 => rev(tail, rev(head2 :: tail2, Nil) :: result)
      case _ => rev(tail, head :: result)
    }
  }
  rev(l, Nil)
}

import org.scalatest._ 
import org.scalatest.matchers._

object deepReverseSpec extends Spec with ShouldMatchers {
  describe ("deepReverse") {
    it ("should return the input list reversed, with nested lists also reversed") {
      reverse(List (List (1, 2), List (3, 4))) should equal (List (List (3, 4), List (1, 2)))
      deepReverse(List (List (1, 2), List (3, 4))) should equal (List (List (4, 3), List (2, 1)))
      deepReverse(List (10, List (1, 2), 20, List (3, 4), 30)) should equal (List (30, List (4, 3), 20, List (2, 1), 10))
      deepReverse(List(1, 2, 3, 4, 5, 6)) should equal (List(6, 5, 4, 3, 2, 1))
      deepReverse(List(1, 2)) should equal (List(2, 1))
      deepReverse(List(1)) should equal (List(1))
      deepReverse(Nil) should equal (Nil)
    }
  }
} 
deepReverseSpec execute

