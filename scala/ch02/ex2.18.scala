def reverse[A](l: List[A]) = {
  def rev(l2: List[A], result: List[A]): List[A] = l2 match {
    case Nil => result
    case _ => rev(l2.tail, l2.head :: result)
  }
  rev(l, Nil)
}

import org.scalatest._ 
import org.scalatest.matchers._

object reverseSpec extends Spec with ShouldMatchers {
  describe ("reverse") {
    it ("should return the input list reversed") {
      reverse(List(1, 2, 3, 4, 5, 6)) should equal (List(6, 5, 4, 3, 2, 1))
      reverse(List(1, 2)) should equal (List(2, 1))
      reverse(List(1)) should equal (List(1))
      reverse(Nil) should equal (Nil)
    }
  }
} 

reverseSpec execute
