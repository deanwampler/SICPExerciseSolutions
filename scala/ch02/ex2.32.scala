def subsets[A](s:List[A]): List[List[A]] = s match {
  case Nil => List(Nil)
  case head :: tail => 
    val rest = subsets(tail)
    rest ::: (rest map {x => head :: x})
}

import org.scalatest._ 
import org.scalatest.matchers._

object subsetsSpec extends Spec with ShouldMatchers {
  describe ("treeMap") {
    it ("should return a Tree with the elements mapped by the input function") {
      subsets (List(1, 2, 3)) should equal ( 
        List[Int]() :: List(3) :: List(2) :: List(2, 3) :: List(1) :: List(1, 3) :: List(1, 2) :: List(List(1, 2, 3)))
    }
  }
}
subsetsSpec execute
