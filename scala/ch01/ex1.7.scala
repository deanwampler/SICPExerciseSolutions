// The original implementation in the book:

def square (x: Double) = x * x

def average (x: Double, y: Double) = (x + y) / 2
  
def abs (x: Double) = Math.abs(x)
  
def improve (guess: Double, x: Double) = average(guess, x / guess)
  
def goodEnough (guess: Double, x: Double) = abs(square(guess) - x) < 0.00001
  
def sqrtIter (guess: Double, x: Double): Double =
  if (goodEnough (guess, x)) guess else sqrtIter(improve(guess, x), x)

def sqrt (x: Double) =  sqrtIter(1.0, x)
  
// Refined implementation

def goodEnough2 (guess: Double, prevGuess: Double, x: Double) =
  abs(guess - prevGuess) < (guess * 0.00001)
  
def sqrtIter2 (guess: Double, prevGuess: Double, x: Double): Double =
  if (goodEnough2(guess, prevGuess, x)) guess else sqrtIter2(improve(guess, x), guess, x)

def sqrt2 (x: Double) = sqrtIter2(1.0, 0.0, x)

import org.scalatest._
import org.scalatest.matchers._

object sqrtSpec extends Spec with ShouldMatchers {
  describe ("Original square root calculator") {
    it ("should do poorly for large numbers compared to the revised calculator") {
      // The original sqrt is actually more accurate for large X, if we use a larger
      // delta (0.001) in the good-enough? and good-enough2? methods. With 0.00001, 
      // they are about the same.
      sqrt(4E16)  should be (2E8 plusOrMinus 1.0)
      sqrt2(4E16) should be (2E8 plusOrMinus 1.0)    
    }
    it ("should do poorly for small numbers compared to the revised calculator") {
      // The original sqrt is horrible for small X
      sqrt(4E-16)  should be (2E-8 plusOrMinus 0.002) // Note the error range!
      sqrt2(4E-16) should be (2E-8 plusOrMinus 1E-12)    
    }
  }
}
sqrtSpec execute
