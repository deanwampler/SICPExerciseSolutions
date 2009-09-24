def aPlusAbsB(a: Int, b: Int) = a + (if(b<0) -b else b)

import org.scalatest._
import org.scalatest.matchers._

object aPlusAbsBSpec extends Spec with ShouldMatchers {
  describe ("aPlusAbsB") {
    it ("should add 'a' to the absolute value of 'b'.") {
      aPlusAbsB(1, -2) should equal (3)
      aPlusAbsB(1, -1) should equal (2)
      aPlusAbsB(1,  0) should equal (1)
      aPlusAbsB(1,  1) should equal (2)
      aPlusAbsB(1,  2) should equal (3)
    }
  }
}

aPlusAbsBSpec execute

