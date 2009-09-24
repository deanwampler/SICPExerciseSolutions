def accumulate[A](combiner: (A,A) => A, null_value: A, 
    term: (Int) => A, a: Int, next: (Int) => (Int), b: Int): A = {
  if (a > b) null_value else
      combiner(term(a), accumulate(combiner, null_value, term, next(a), next, b))
}

def accumulate2[A](combiner: (A,A) => A, null_value: A, 
    term: (Int) => A, a: Int, next: (Int) => (Int), b: Int) = {
  def ac(a2: Int, accum: A): A = if (a2 > b) accum else
      ac(next(a2), combiner(term(a2), accum))
  ac(a, null_value)
}

def prod(term: (Int) => Int, a: Int, next: (Int) => (Int), b: Int) = 
  accumulate((i:Int,j:Int) => i*j, 1, term, a, next, b)

def prod2(term: (Int) => Int, a: Int, next: (Int) => (Int), b: Int): Int = 
  accumulate2((i:Int,j:Int) => i*j, 1, term, a, next, b)

def sum(term: (Int) => Double, a: Int, next: (Int) => (Int), b: Int) = 
  accumulate((x:Double,y:Double) => x+y, 0.0, term, a, next, b)

def sum2(term: (Int) => Double, a: Int, next: (Int) => (Int), b: Int): Double = 
  accumulate2((x:Double,y:Double) => x+y, 0.0, term, a, next, b)

def even(n: Int) = n % 2 == 0
val cube = (n: Double) => n * n * n
def identity[A](n: A) = n
def inc(n: Int) = n + 1

def fact(n: Int)  = prod(identity, 1, inc, n)
def fact2(n: Int) = prod2(identity, 1, inc, n)

def simpsonsRule(f: (Double)=>Double, a: Double, b: Double, n: Int) = {
  val h = (b - a) / n
  def fy(k: Int) = f(a + (k * h))
  def term(i: Int) = fy(i) * (if (even(i)) 2 else 4)
  (h / 3) * (fy(0) + sum(term, 1, inc, (n - 1)) + fy(n))
}
def simpsonsRule2(f: (Double)=>Double, a: Double, b: Double, n: Int) = {
  val h = (b - a) / n
  def fy(k: Int) = f(a + (k * h))
  def term(i: Int) = fy(i) * (if (even(i)) 2 else 4)
  (h / 3) * (fy(0) + sum2(term, 1, inc, (n - 1)) + fy(n))
}

import org.scalatest._
import org.scalatest.matchers._

val factVals = Array(1, 2, 6, 24, 120, 720, 5040)
  
object factorialSpec extends Spec with ShouldMatchers {
  describe ("Factorials computed with recursive 'accumulate'") {
    it ("should calculate the correct values") {
      (1 to 7) foreach { n => fact(n) should equal (factVals(n-1)) }
    }
  }
  describe ("Factorials computed with iterative 'accumulate'") {
    it ("should calculate the correct values") {
      (1 to 7) foreach { n => fact2(n) should equal (factVals(n-1)) }
    }
  }
}

factorialSpec execute

object simpsonsRuleSpec extends Spec with ShouldMatchers {
  describe ("simpsonsRule computed with recursive 'accumulate'") {
    it ("should converge to the correct answer with more and more steps") {
      simpsonsRule(cube, 0.0, 1.0, 10)   should be (.25 plusOrMinus 0.01)
      simpsonsRule(cube, 0.0, 1.0, 100)  should be (.25 plusOrMinus 0.001)
      simpsonsRule(cube, 0.0, 1.0, 1000) should be (.25 plusOrMinus 0.0001)
      // Stack overflow!
      // simpsonsRule(cube, 0.0, 1.0, 10000) should be (.25 plusOrMinus 0.00001)
    }
  }
  describe ("simpsonsRule computed with iterative 'accumulate'") {
    it ("should converge to the correct answer with more and more steps") {
      simpsonsRule2(cube, 0.0, 1.0, 10)   should be (.25 plusOrMinus 0.01)
      simpsonsRule2(cube, 0.0, 1.0, 100)  should be (.25 plusOrMinus 0.001)
      simpsonsRule2(cube, 0.0, 1.0, 1000) should be (.25 plusOrMinus 0.0001)
      // No Stack overflow!
      simpsonsRule2(cube, 0.0, 1.0, 10000) should be (.25 plusOrMinus 0.00001)
    }
  }
}

simpsonsRuleSpec execute

