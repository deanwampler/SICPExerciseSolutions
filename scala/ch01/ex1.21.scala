def square(n: Int) = n * n

def findDivisor(n: Int, testDivisor: Int): Int = {
  if (square(testDivisor) > n) n
  else if (divides(testDivisor, n)) testDivisor
  else findDivisor(n, testDivisor + 1)
}

def smallestDivisor(n: Int) = findDivisor(n, 2)

def divides(testDivisor: Int, n: Int) = n % testDivisor == 0

def prime(n: Int) = smallestDivisor(n) == n

// Print results:
List(199, 1999, 19999) foreach { n => 
  val div = smallestDivisor (n)
  println(n+": "+div+", prime? "+prime(n))
}

import org.scalatest._
import org.scalatest.matchers._

// Test results:
object primeSpec extends Spec with ShouldMatchers {
  describe ("prime calculator") {
    it ("should find primes") {
      prime(199)   should equal (true)
      prime(1999)  should equal (true)
      prime(19999) should equal (false)
    }
  }
  describe ("smallestDivisor") {
    it ("should find the smallest divisor") {
      smallestDivisor(199)   should equal (199)
      smallestDivisor(1999)  should equal (1999)
      smallestDivisor(19999) should equal (7)
    }
  }
}

primeSpec execute
