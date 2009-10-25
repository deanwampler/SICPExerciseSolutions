def bothEven(x:Int, y:Int) = (x % 2 == 0) && (y % 2 == 0)
def bothOdd(x:Int, y:Int)  = (x % 2 == 1) && (y % 2 == 1)

def sameParity(p:Int, l:Int*) = {
  def sm(l2:List[Int], answer:List[Int]): List[Int] = l2 match {
    case Nil => answer
    case _ if (bothEven(p, l2.head) || bothOdd(p, l2.head)) => sm(l2.tail, l2.head :: answer)
    case _ => sm(l2.tail, answer)
  }
  sm(l.toList, p :: Nil) reverse
}

import org.scalatest._ 
import org.scalatest.matchers._

object sameParitySpec extends Spec with ShouldMatchers {
  describe ("sameParity") {
    it ("should return a list with the parity element if only the parity element is given") {
      sameParity(1) should equal (List(1))
      sameParity(2) should equal (List(2))
    }
    it ("should return a list with the parity element and all subsequent elements with the same parity") {
      sameParity(1, 2, 3, 4, 5, 6, 7) should equal (List(1, 3, 5, 7))
      sameParity(2, 3, 4, 5, 6, 7) should equal (List(2, 4, 6))
      sameParity(3, 4, 5, 6, 7) should equal (List(3, 5, 7))
      sameParity(4, 5, 6, 7) should equal (List(4, 6))
    }
  }
}
sameParitySpec execute