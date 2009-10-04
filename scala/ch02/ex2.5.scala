def even (n: Int) = n % 2 == 0
def square (n: Int) = n * n

def fastExpt (base: Int, n: Int): Int = n match {
    case 0 => 1
    case _ if even(n) => square(fastExpt(base, n/2))
    case _ => base * fastExpt(base, n - 1)
  }

type expPair = Int

// To find a, keep dividing z by 2, counting the number of times.
def findA (z: expPair) = {
  def f (a: Int, leftOver: expPair): Int = 
    if (leftOver % 2 == 0) f(a + 1, leftOver / 2) else a
  f(0, z)
}

// To find b, keep dividing z by 3, counting the number of times.
def findB (z: expPair) = {
  def f (b: Int, leftOver: expPair): Int = 
    if (leftOver % 3 == 0) f(b + 1, leftOver / 3) else b
  f(0, z)
}

def cons (a: Int, b: Int) = fastExpt(2, a) * fastExpt(3, b)

def car (z: expPair) = findA(z)
def cdr (z: expPair) = findB(z)

import org.scalatest._
import org.scalatest.matchers._

object expPairSpec extends Spec with ShouldMatchers {
  val zeroZero = cons(0, 0)
  val oneTwo   = cons(1, 2)
  val twoOne   = cons(2, 1)
  val fourFive = cons(4, 5)

  describe ("car on a pair, implemented as (2**a)*(3**b)") {
    it ("should return a") {
      car(zeroZero) should equal (0)
      car(oneTwo)   should equal (1)
      car(twoOne)   should equal (2)
      car(fourFive) should equal (4)
    }
  }

  describe ("cdr on a pair, implemented as (2**a)*(3**b)") {
    it ("should return b") {
      cdr(zeroZero) should equal (0)
      cdr(oneTwo)   should equal (2)
      cdr(twoOne)   should equal (1)
      cdr(fourFive) should equal (5)
    }
  }
}
expPairSpec execute
