def cube(x: Double) = x * x * x

var count = 0
def p(x: Double) = {
  count += 1
  (3 * x) - (4 * cube(x))
}

def sine(angle: Double): Double = 
  if (math.abs(angle) < 0.1) angle else p(sine(angle / 3))

sine(12.15)

import org.scalatest._
import org.scalatest.matchers._

object sineSpec extends Spec with ShouldMatchers {
  describe ("sine formula") {
    it ("should take 5 iterations for sine(12.15)") {
      count should equal (5)
    }
  }
}

sineSpec execute
