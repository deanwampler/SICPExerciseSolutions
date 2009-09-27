def compose[T1, T2, T3] (f: (T2) => T3, g: (T1) => T2) = (x:T1) => f(g(x))

def square (n: Int) = n * n
def inc (n: Int) = n + 1

import org.scalatest._
import org.scalatest.matchers._

object composeSpec extends Spec with ShouldMatchers {
  describe ("compose") {
    it ("should compose two functions") {
      compose(square, inc)(6) should equal (49)
      compose(inc, square)(6) should equal (37)
    }
  }
}
composeSpec execute
