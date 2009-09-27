// solving cubic equations

val dx = 0.00001

// Could import these Math functions, but we get type ambiguities (?)
def abs(x: Double) = Math.abs(x)

def closeEnough(x: Double, y: Double) = abs(x - y) < dx

def fixedPoint(f: (Double) => Double, firstGuess: Double) = {
  def fp(guess: Double, count: Int): Double = {
    val nextGuess = f(guess)
    if (closeEnough(guess, nextGuess))
      nextGuess
    else
      fp(nextGuess, count+1)
  }
  fp(firstGuess, 1)
}

def deriv(g: (Double) => Double) = (x:Double) =>
  (g(x + dx) - g(x)) / dx
                
def newtonTransform (g: (Double) => Double) = (x: Double) =>
  (x - (g(x) / deriv(g)(x)))
  
def newtonsMethod (g: (Double) => Double, guess: Double) =
  fixedPoint (newtonTransform(g), guess)
  
def cubic (a: Double, b: Double, c: Double) = (x:Double) =>
  (x * x * x) + (a * x * x) + (b * x) + c

println (newtonsMethod (cubic (0, 0, 1), 1.0))    // -0.9999999999999863
println (newtonsMethod (cubic (0, 1, 0), 1.0))    // 3.668097353908429e-17
println (newtonsMethod (cubic (1, 0, 0), 1.0))    // 1.1227429100448376e-05
println (newtonsMethod (cubic (1, 1, -14), 1.0))  // 2.0000000000000133
println (newtonsMethod (cubic (2, 1, 1), 1.0))    // -1.7548776662280976
println (newtonsMethod (cubic (1, 2, 1), 1.0))    // -0.5698402909980529
println (newtonsMethod (cubic (1, 1, 2), 1.0))    // -1.3532099641952162

import org.scalatest._
import org.scalatest.matchers._

object newtonsMethodSpec extends Spec with ShouldMatchers {
  describe ("Newton's method") {
    it ("should converge on the solution applied to cubic equations") {
      newtonsMethod (cubic (0, 0, 1), 1.0) should be (-1.0 plusOrMinus dx)
      newtonsMethod (cubic (0, 1, 0), 1.0) should be (0.0 plusOrMinus dx)
      // This one probably has significant round-off errors:
      newtonsMethod (cubic (1, 0, 0), 1.0) should be (0.0 plusOrMinus (10.0*dx))
      newtonsMethod (cubic (1, 1, -14), 1.0) should be (2.0 plusOrMinus dx)
    }
  }
}
newtonsMethodSpec execute