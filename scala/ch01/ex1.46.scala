val tolerance = 0.00001

def square(x: Double) = x*x

def iterativeImprove(goodEnough: (Double) => Boolean, improve: (Double) => Double) = {
  def iterate(guess2: Double): Double = goodEnough(guess2) match {
    case true => guess2
    case _ => iterate(improve(guess2))
  }
  (guess:Double) => iterate(guess)
}

def average(x: Double, y: Double) = (x + y) / 2.0

def goodEnough(guess: Double, x: Double) = Math.abs(square(guess) - x) < tolerance

def improve(guess: Double, x: Double) = average(guess, x / guess)
  
def sqrt(x: Double) = iterativeImprove(
  (guess:Double) => goodEnough(guess, x), 
  (guess:Double) => improve(guess, x))(1.0)
    
import org.scalatest._
import org.scalatest.matchers._

object iterativeImproveForSqrtSpec extends Spec with ShouldMatchers {
  describe ("nth-power calculator") {
    it ("should calculate the correct values") {
      sqrt(2)    should be (1.414213562373095 plusOrMinus 0.0001)
      sqrt(3)    should be (1.732050807568877 plusOrMinus 0.0001)
      sqrt(4)    should be (2.0               plusOrMinus 0.0001)
      sqrt(4E16) should be (2E8               plusOrMinus 1.0)
    }
  }
}
iterativeImproveForSqrtSpec execute
