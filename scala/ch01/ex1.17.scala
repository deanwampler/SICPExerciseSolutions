def doub(n: Int) = n * 2
def halve(n: Int) = n / 2
def even(n: Int) = n % 2 == 0
def mult(a: Int, n: Int) = {
  def mult2(b: Int, m: Int): Int = m match {
    case 0 => 0
    case _ if (even(m)) => mult2(doub(b), halve(m))
    case _ => b + mult2(b, m-1)
  }
  mult2(a,n)
}

import org.scalatest._
import org.scalatest.matchers._

object multSpec extends Spec with ShouldMatchers {
  describe ("multiplication formula") {
    it ("should work ;)") {
      (0 to 20) foreach { n => mult(2, n) should equal (2*n) }
    }
  }
}

multSpec execute
