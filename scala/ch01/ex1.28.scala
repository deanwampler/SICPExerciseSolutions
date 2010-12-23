// Miller-Rabin Test

def square (n: Int) = n * n
def random(n: Long) = (math.random * n) toInt

def mrTest (base: Int, exp: Int, m: Int) = base match {
  case 1 => false
  case _ if (base == m - 1) => false
  case _ => square(base) == (1 % m)
}

def expmod (base: Int, exp: Int, m: Int): Int = exp match {
  case 0 => 1
  case _ if (exp % 2 == 0) => {
    mrTest(base, exp, m) match {
      case true  => 0
      case false => square(expmod(base, exp / 2, m)) % m
    }
  }
  case _ => (base * expmod(base, exp - 1, m)) % m
}

def fermatTest(n: Int) = {
  def tryIt(a: Int) = expmod(a, n, n) == n
  tryIt(1 + random(n - 1))
}

def fastPrime(n: Int, times: Int): Boolean = {
  if (times == 0)
    true
  else if (fermatTest(n))
    fastPrime(n, times - 1)
  else
    false
}

import org.scalatest._
import org.scalatest.matchers._

// Test results:
object carmichaelNumbersSpec extends Spec with ShouldMatchers {
  describe ("fermatTest with Miller-Rabin test") {
    it ("should detect that Carmichael numbers are not prime") {
      fastPrime(561,  1000) should equal (false)
      fastPrime(1105, 1000) should equal (false)
      fastPrime(1729, 1000) should equal (false)
      fastPrime(2465, 1000) should equal (false)
      fastPrime(2821, 1000) should equal (false)
      fastPrime(6601, 1000) should equal (false)
    }
  }
}
carmichaelNumbersSpec execute
  

