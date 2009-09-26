// Fixed point, again

val tolerance = 0.00001

// Could import these Math functions, but we get type ambiguities (?)
def abs(x: Double) = Math.abs(x)
def log(x: Double) = Math.log(x)

def closeEnough(x: Double, y: Double) = abs(x - y) < tolerance

def average(x: Double, y: Double) = (x + y) / 2.0

def fixedPoint(f: (Double) => Double, firstGuess: Double) = {
  def fp(guess: Double, count: Int): Double = {
    val nextGuess = f(guess)
    if (closeEnough(guess, nextGuess)) {
      println(count)
      nextGuess
    } else
      fp(nextGuess, count+1)
  }
  fp(firstGuess, 1)
}

import org.scalatest._
import org.scalatest.matchers._

object fixedPointSpec extends Spec with ShouldMatchers {
  describe ("fixedPoint") {
    it ("should converge on the solution") {
      fixedPoint(x => log(1000.0) / log(x), 10.0) should be (4.55553 plusOrMinus tolerance)
    }
  }
  describe ("fixedPoint with averaging") {
    it ("should converge on the solution faster") {
      fixedPoint(x => average(x, log(1000.0) / log(x)), 10.0) should be (4.55553 plusOrMinus tolerance)
    }
  }
}

fixedPointSpec execute
