def sum(term: (Int) => Double, m: Int, next: (Int)=>Int, n: Int): Double = 
  if (m > n) 0 else term(m) + sum(term, next(m), next, n)
    
def inc(n: Int) = n + 1
def even(n: Int) = n % 2 == 0

def simpsonsRule(f: (Double)=>Double, a: Double, b: Double, n: Int) = {
  val h = (b - a) / n
  def fy(k: Int) = f(a + (k * h))
  def term(i: Int) = fy(i) * (if (even(i)) 2 else 4)
  (h / 3) * (fy(0) + sum(term, 1, inc, (n - 1)) + fy(n))
}

val cube = (n: Double) => n * n * n

import org.scalatest._
import org.scalatest.matchers._

object simpsonsRuleSpec extends Spec with ShouldMatchers {
  describe ("simpsonsRule") {
    it ("should converge to the correct answer with more and more steps") {
      simpsonsRule(cube, 0.0, 1.0, 10)   should be (.25 plusOrMinus 0.01)
      simpsonsRule(cube, 0.0, 1.0, 100)  should be (.25 plusOrMinus 0.001)
      simpsonsRule(cube, 0.0, 1.0, 1000) should be (.25 plusOrMinus 0.0001)
      // Stack overflow (but see 1.30)
      // simpsonsRule(cube, 0.0, 1.0, 10000) should be (.25 plusOrMinus 0.00001)
    }
  }
}

simpsonsRuleSpec execute
