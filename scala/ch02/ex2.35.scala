def accumulate[A, B] (op: (A, B) => B, 
                      initial: B, sequence: List[A]): B =
  sequence match {
    case Nil => initial
    case _ => op (sequence.head, accumulate(op, initial, sequence.tail))
  }

def countLeaves (tree: List[_]): Int =
  accumulate ((node:Any, count:Int) => node match {
    case l:List[_] => count + countLeaves(l)
    case i:Int     => count + 1
    case _         => throw new RuntimeException("Invalid node type: "+node) 
  }, 0, tree)

val x  = List (List(1, 2), List(3, 4))
val xx = List (x, x)


import org.scalatest._ 
import org.scalatest.matchers._

object countLeavesSpec extends Spec with ShouldMatchers {
  describe ("countLeaves defined using accumulate") {
    it ("should return the number of leaf nodes in a tree") {
      countLeaves(x)  should equal (4)
      countLeaves(xx) should equal (8)
    }
  }
}
countLeavesSpec execute
