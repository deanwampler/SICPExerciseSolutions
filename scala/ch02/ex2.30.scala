
def squareTree1(tree: List[_]): List[_] = tree match {
  case Nil => Nil
  case head :: tail => head match {
    case l:List[_] => squareTree1(l) :: squareTree2(tail)
    case i:Int => i * i :: squareTree2(tail)
    case x => throw new RuntimeException("unknown type of list element: " + x)
  }
}
    
def squareTree2 (tree: List[_]): List[_] = tree map {
  case l:List[_] => squareTree2(l)
  case i:Int => i * i
  case x => throw new RuntimeException("unknown type of list element: " + x)
}

import org.scalatest._ 
import org.scalatest.matchers._

object squareTreeSpec extends Spec with ShouldMatchers {
  describe ("squareTree1") {
    it ("should return a Tree with the elements squared") {
      squareTree1(
        List (1, List (2, List (3, 4), 5), List (6, 7))) should equal ( 
        List (1, List (4, List (9, 16), 25), List (36, 49)))
      squareTree1(Nil) should equal (Nil)
      squareTree1(List(2)) should equal (List(4))
    }
  }
  describe ("squareTree2") {
    it ("should return a Tree with the elements squared") {
      squareTree2(
        List (1, List (2, List (3, 4), 5), List (6, 7))) should equal ( 
        List (1, List (4, List (9, 16), 25), List (36, 49)))
      squareTree2(Nil) should equal (Nil)
      squareTree2(List(2)) should equal (List(4))
    }
  }
}  
squareTreeSpec execute
