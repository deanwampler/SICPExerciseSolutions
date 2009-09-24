// Carmichael numbers

def square (n: Int) = n * n

def expmod (base: Int, exp: Int, m: Int): Int = exp match {
  case 0 => 1
  case _ if (exp % 2 == 0) => square(expmod(base, exp / 2, m)) % m
  case _ => (base * expmod(base, exp - 1, m)) %m
}

def congruent(n: int) = {
  def c(n: Int, a: Int): Boolean = if (a == n)
    true
  else if (expmod(a, n, n) == (a % n))
    c(n, a + 1)
  else
    false
  c(n, 1)
}


import org.scalatest._
import org.scalatest.matchers._

// Test results:
object carmichaelNumbersSpec extends Spec with ShouldMatchers {
  describe ("Congruence test") {
    it ("should pass for Carmichael numbers") {
      congruent(561)  should equal (true)
      congruent(1105) should equal (true)
      congruent(1729) should equal (true)
      congruent(2465) should equal (true)
      congruent(2821) should equal (true)
      congruent(6601) should equal (true)
    }
  }
}
carmichaelNumbersSpec execute
  

