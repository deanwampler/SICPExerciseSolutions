def fib3(n: Int): Int = n match {
  case _ if (n < 3) => n
  case _ => fib3(n-1) + fib3(n-2) + fib3(n-3)
}

def fib3b(n: Int): Int = {
  def f(i: Int, a: Int, b: Int, c: Int): Int = i match {
    case _ if (n < 3) => n
    case _ if (i == n) => a+b+c
    case _ => f(i + 1, b, c, a+b+c)
  }
  f(3, 0, 1, 2)
}

import org.scalatest._
import org.scalatest.matchers._

val fib3vals = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 6, 5 -> 11, 6 -> 20, 7 -> 37)

object fib3Spec extends Spec with ShouldMatchers {
  describe ("fib3") {
    it ("should calculate fib3(n) recursively") {
      fib3vals.keys foreach { n => fib3(n) should equal (fib3vals(n)) }
    }
  }
  describe ("fib3b") {
    it ("should calculate fib3(n) iteratively") {
      fib3vals.keys foreach { n => fib3b(n) should equal (fib3vals(n)) }
    }
  }
}

fib3Spec execute
