
// Contrast with ex. 2.30, where we matched on i:Int, and x, where the "x" would
// something unknown. Unfortunately, if you try "a:A", you get an erasure warning,
// so we let "a" match anything and then cast to A.
def treeMap[A] (f: A => A, tree: List[_]): List[_] = tree map {
  case l:List[_] => treeMap(f, l)
  case a => f(a.asInstanceOf[A])
}

def squareTree(tree: List[_]): List[_] = treeMap[Int](i => i*i, tree)

import org.scalatest._ 
import org.scalatest.matchers._

object treeMapSpec extends Spec with ShouldMatchers {
  describe ("treeMap") {
    it ("should return a Tree with the elements mapped by the input function") {
      squareTree(
        List (1, List (2, List (3, 4), 5), List (6, 7))) should equal ( 
        List (1, List (4, List (9, 16), 25), List (36, 49)))
      squareTree(Nil) should equal (Nil)
      squareTree(List(2)) should equal (List(4))
    }
  }
}  
treeMapSpec execute
