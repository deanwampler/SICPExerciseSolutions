def squareList1 (items:List[Int]):List[Int] = items match {
  case Nil => Nil
  case _ => (items.head * items.head) :: squareList1 (items.tail)
}
    
def squareList2 (items:List[Int]) = items map (x => x * x)

import org.scalatest._ 
import org.scalatest.matchers._

object squareListSpec extends Spec with ShouldMatchers {
  describe ("squareList1") {
    it ("should return a list with the elements squared") {
      squareList1(List(1, 2, 3, 4, 5)) should equal (List(1, 4, 9, 16, 25))      
    }
  }
  describe ("squareList2") {
    it ("should return a list with the elements squared") {
      squareList2(List(1, 2, 3, 4, 5)) should equal (List(1, 4, 9, 16, 25))      
    }
  }
}  
squareListSpec execute
