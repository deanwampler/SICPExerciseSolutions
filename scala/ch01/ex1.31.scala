def prod(term: (Int) => Int, a: Int, next: (Int) => (Int), b: Int) = {
  def p(a2: Int, accum: Int): Int = if (a2 > b) accum else
      p(next(a2), term(a2) * accum)
  p(a, 1)
}

def prod2(term: (Int) => Int, a: Int, next: (Int) => (Int), b: Int): Int = 
  if (a > b) 1 else 
    (term(a) * prod2(term, next(a), next, b))

def identity[A](n: A) = n
def inc(n: Int) = n + 1

def fact(n: Int) = prod(identity, 1, inc, n)
def fact2(n: Int) = prod2(identity, 1, inc, n)

import org.scalatest._
import org.scalatest.matchers._

val factVals = Array(1, 2, 6, 24, 120, 720, 5040)
  
object factorialSpec extends Spec with ShouldMatchers {
  describe ("Factorials computed with recursive 'prod'") {
    it ("should calculate the correct values") {
      (1 to 7) foreach { n => fact(n) should equal (factVals(n-1)) }
    }
  }
  describe ("Factorials computed with iterative 'prod'") {
    it ("should calculate the correct values") {
      (1 to 7) foreach { n => fact2(n) should equal (factVals(n-1)) }
    }
  }
}

factorialSpec execute
