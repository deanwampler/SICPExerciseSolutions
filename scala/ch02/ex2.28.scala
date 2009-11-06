
def fringe(l: List[_]): List[_] = {
  def f(l2: List[_], result: List[_]): List[_] = l2 match {
    case Nil => result
    case head :: tail => head match {
      case head2 :: tail2 => f(tail, f(head2 :: tail2, Nil) ::: result)
      case _ => f(tail, head :: result)
    }
  }
  f(l, Nil) reverse
}

import org.scalatest._ 
import org.scalatest.matchers._

object fringeSpec extends Spec with ShouldMatchers {
  describe ("fringe") {
    it ("should return a flat list with the leaf nodes in depth-first order") {
      fringe(List (List (1, 2), List (3, 4))) should equal (List (1, 2, 3, 4))
      fringe(List (10, List (1, 2), 20, List (3, 4), 30)) should equal (List (10, 1, 2, 20, 3, 4, 30))
      fringe(List(1, 2, 3, 4, 5, 6)) should equal (List(1, 2, 3, 4, 5, 6))
      fringe(List(1, 2)) should equal (List(1, 2))
      fringe(List(1)) should equal (List(1))
      fringe(Nil) should equal (Nil)
    }
  }
} 
fringeSpec execute

