// Newton's Method for Cubes, using the approach in 1.7.

def cube (x: Double) = x * x * x

def abs (x: Double) = math.abs(x)
  
def improve (guess: Double, x: Double) = 
  ((x / (guess * guess)) + (2.0 * guess)) / 3.0
  
def goodEnough (guess: Double, prevGuess: Double, x: Double) = 
  abs(guess - prevGuess) < (guess * 0.00001)

def cubeRootIter (guess: Double, prevGuess: Double, x: Double): Double =
  if (goodEnough(guess, prevGuess, x)) guess else cubeRootIter(improve(guess, x), guess, x)

def cubeRoot (x: Double) = cubeRootIter(1.0, 0.0, x)

import org.scalatest._
import org.scalatest.matchers._

object cubeRootSpec extends Spec with ShouldMatchers {
  describe ("Cube root calculator") {
    it ("should compute cube roots") {
      cubeRoot(8E15)  should be (2E5 plusOrMinus 1.0)
      cubeRoot(8E-15) should be (2E-5 plusOrMinus 1.0)
    }
  }
}
cubeRootSpec execute
