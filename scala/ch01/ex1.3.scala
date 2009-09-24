
def f(n1 :Int, n2: Int, n3: Int) = (n1 :: n2 :: n3 :: Nil) sort {
  (a,b) => a > b } match {
    case a :: b :: _ => ((a * a) + (b * b))
    case _ => throw new Exception("Must specify 2 or more numbers")
  }

import org.scalatest._
import org.scalatest.matchers._

object squareAndAddTopTwoSpec extends Spec with ShouldMatchers {

  describe ("Squaring top two of three integers") {

    it ("should select the top two values, square them, and add the result") {
      f (2, 3, 4) should equal (25)
      f (2, 4, 3) should equal (25)
      f (4, 3, 2) should equal (25)
      f (3, 4, 2) should equal (25)
      f (4, 2, 3) should equal (25)
      f (3, 2, 4) should equal (25)
    }
  }
}

squareAndAddTopTwoSpec execute
