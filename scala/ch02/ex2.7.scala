type N = Double  // Shorthand
type Interval = Tuple2[N,N]

def makeInterval (a:N, b:N) = (a, b)
def lowerBound (x: Interval) = x._1
def upperBound (x: Interval) = x._2

def addInterval (x: Interval, y: Interval) =
  makeInterval (lowerBound(x) + lowerBound(y), upperBound(x) + upperBound(y))

def min(ns: N*) = {
  val sorted = (util.Sorting.stableSort(ns))
  sorted(0)
}
def max(ns: N*) = {
  val sorted = (util.Sorting.stableSort(ns)).reverse
  sorted(0)
}

def mulInterval (x: Interval, y: Interval) = {
  val p1 = lowerBound(x) * lowerBound(y)
  val p2 = lowerBound(x) * upperBound(y)
  val p3 = upperBound(x) * lowerBound(y)
  val p4 = upperBound(x) * upperBound(y)
  makeInterval (min(p1, p2, p3, p4), max(p1, p2, p3, p4))
}
    
def divInterval (x: Interval, y: Interval) =
  mulInterval (x, makeInterval (1.0 / upperBound(y), 1.0 / lowerBound(y))) 
                                 
val minusOneOneInterval = makeInterval (-1.0, 1.0)
val zeroOneInterval     = makeInterval ( 0.0, 1.0)
val twoThreeInterval    = makeInterval ( 2.0, 3.0)

val add1 = addInterval (minusOneOneInterval, zeroOneInterval)
val add2 = addInterval (minusOneOneInterval, twoThreeInterval)
val add3 = addInterval (zeroOneInterval,     twoThreeInterval)


import org.scalatest._
import org.scalatest.matchers._

object addIntervalSpec extends Spec with ShouldMatchers {
  describe ("addInterval") {
    it ("should return the sum of the lower bounds and upper bounds") {
      lowerBound(add1)  should equal (-1.0)
      upperBound(add1)  should equal (2.0)
      lowerBound(add2)  should equal (1.0)
      upperBound(add2)  should equal (4.0)
      lowerBound(add3)  should equal (2.0)
      upperBound(add3)  should equal (4.0)
    }
  }
}
addIntervalSpec execute

val mul1 = mulInterval (minusOneOneInterval, zeroOneInterval)
val mul2 = mulInterval (minusOneOneInterval, twoThreeInterval)
val mul3 = mulInterval (zeroOneInterval,     twoThreeInterval)

object mulIntervalSpec extends Spec with ShouldMatchers {
  describe ("addInterval") {
    it ("should return the sum of the lower bounds and upper bounds") {
      lowerBound(mul1)  should equal (-1.0)
      upperBound(mul1)  should equal (1.0)
      lowerBound(mul2)  should equal (-3.0)
      upperBound(mul2)  should equal (3.0)
      lowerBound(mul3)  should equal (0.0)
      upperBound(mul3)  should equal (3.0)
    }
  }
}
mulIntervalSpec execute

val div1 = divInterval (zeroOneInterval,     minusOneOneInterval)
val div2 = divInterval (minusOneOneInterval, twoThreeInterval)
val div3 = divInterval (zeroOneInterval,     twoThreeInterval)

object divIntervalSpec extends Spec with ShouldMatchers {
  describe ("addInterval") {
    it ("should return the sum of the lower bounds and upper bounds") {
      lowerBound(div1)  should equal (-1.0)
      upperBound(div1)  should equal (1.0)
      lowerBound(div2)  should equal (-0.5)
      upperBound(div2)  should equal (0.5)
      lowerBound(div3)  should equal (0.0)
      upperBound(div3)  should equal (0.5)
    }
  }
}
divIntervalSpec execute
