// Calculate the golden ratio, phi.

val tolerance = 0.00001

def abs(x: Double) = math.abs(x)

def closeEnough(x: Double, y: Double) = abs(x - y) < tolerance

def average(x: Double, y: Double) = (x + y) / 2.0

def fixedPoint(f: (Double) => Double, firstGuess: Double) = {
  def fp(guess: Double): Double = {
    val nextGuess = f(guess)
    if (closeEnough(guess, nextGuess))
      nextGuess
    else
      fp(nextGuess)
  }
  fp(firstGuess)
}

import org.scalatest._
import org.scalatest.matchers._

object fixedPointSpec extends Spec with ShouldMatchers {
  describe ("fixedPoint") {
    it ("should converge on the solution") {
      fixedPoint(x => 1.0 + (1.0 / x), 1.0) should equal (1.6180327868852458)
    }
  }
}

fixedPointSpec execute
