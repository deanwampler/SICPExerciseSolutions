def double[T] (f: (T) => T) = (x:T) => f(f(x))

def inc (n: Int) = n + 1

import org.scalatest._
import org.scalatest.matchers._

object doubleSpec extends Spec with ShouldMatchers {
  describe ("double") {
    it ("should double the application of a function") {
      double(inc)(2) should equal (4)
      // Without the explicit [Int] parameter on the intermost double, it infers
      // "T" to be Nothing.
      double(double(double[Int]))(inc)(5) should equal (21)
    }
  }
}
doubleSpec execute
