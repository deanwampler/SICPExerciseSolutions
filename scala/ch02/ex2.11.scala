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

def mulIntervalOld (x: Interval, y: Interval) = {
  val p1 = lowerBound(x) * lowerBound(y)
  val p2 = lowerBound(x) * upperBound(y)
  val p3 = upperBound(x) * lowerBound(y)
  val p4 = upperBound(x) * upperBound(y)
  makeInterval (min(p1, p2, p3, p4), max(p1, p2, p3, p4))
}

// Truth table: 
// - indicates < 0, + indicates >= 0
//   xlow xup ylow yup     lower-mult,  upper-mult)
// 1  -    -   -    -      (xup * yup,   xlow * ylow)
// 2  -    -   -    +      (xlow * yup,  xlow * ylow)
// 3  -    -   +    +      (xlow * yup,  xup * ylow)
// 4  -    +   -    -      (xup * ylow,   xlow * ylow)
// 5  -    +   -    +      (min(xlow*yup, xup*ylow), max(xlow*ylow,xup*yup))
// 6  -    +   +    +      (xlow * yup,  xup * yup)
// 7  +    +   -    -      (xup * yup,   xup * ylow)
// 8  +    +   -    +      (xup * ylow,  xup * yup)
// 9  +    +   +    +      (xlow * ylow, xup * yup)

type comp = (N, N) => Boolean

def mulInterval (x: Interval, y: Interval) = {
  def bounds (lx: comp, ux: comp, ly: comp, uy: comp) =
    lx (lowerBound(x), 0) && ux (upperBound(x), 0) && 
    ly (lowerBound(y), 0) && uy (upperBound(y), 0)

  def mkInterval (a:N, b:N, c:N, d:N) = makeInterval(a*b, c*d)

  if (bounds(_ < _, _ < _, _ < _, _ < _))
    mkInterval (upperBound(x), upperBound(y), lowerBound(x), lowerBound(y))
  else if (bounds (_ < _, _ < _, _ < _, _ >= _))
    mkInterval (lowerBound(x), upperBound(y), lowerBound(x), lowerBound(y))
  else if (bounds (_ < _, _ < _, _ >= _, _ >= _))
    mkInterval (lowerBound(x), upperBound(y), upperBound(x), lowerBound(y))
  else if (bounds (_ < _, _ >= _, _ < _, _ < _))
    mkInterval (upperBound(x), lowerBound(y), lowerBound(x), lowerBound(y))
  else if (bounds (_ < _, _ >= _, _ < _, _ >= _)) {
    val p1 = lowerBound(x) * upperBound(y)
    val p2 = upperBound(x) * lowerBound(y)
    val p3 = lowerBound(x) * lowerBound(y)
    val p4 = upperBound(x) * upperBound(y)
    makeInterval (min (p1, p1), max (p3, p4))
  }
  else if (bounds (_ < _, _ >= _, _ >= _, _ >= _))
    mkInterval (lowerBound(x), upperBound(y), upperBound(x), upperBound(y))
  else if (bounds (_ >= _, _ >= _, _ < _, _ < _))
    mkInterval (upperBound(x), upperBound(y), upperBound(x), lowerBound(y))
  else if (bounds (_ >= _, _ >= _, _ < _, _ >= _))
    mkInterval (upperBound(x), lowerBound(y), upperBound(x), upperBound(y))
  else   
    mkInterval (lowerBound(x), lowerBound(y), upperBound(x), upperBound(y))
}

val minusThreeMinusTwoInterval = makeInterval (-3.0, -2.0)
val minusTwoMinusOneInterval = makeInterval (-2.0, -1.0)
val minusTwoTwoInterval  = makeInterval (-2.0, 2.0)
val minusOneOneInterval  = makeInterval (-1.0, 1.0)
val zeroOneInterval      = makeInterval ( 0.0, 1.0)
val twoThreeInterval     = makeInterval ( 2.0, 3.0)
val fourFiveInterval     = makeInterval ( 4.0, 5.0)

import org.scalatest._
import org.scalatest.matchers._

object newMulIntervalSpec extends Spec with ShouldMatchers {
  def equalMulIntervals (x: Interval, y: Interval) = {
    val expected = mulIntervalOld (x, y)
    val actual   = mulInterval (x, y)
    lowerBound(actual) should equal (lowerBound(expected))
    upperBound(actual) should equal (upperBound(expected))
  }
  describe ("new mulInterval") {
    it ("should compute the same result as the old mulInterval") {
      // case 1
      equalMulIntervals (minusThreeMinusTwoInterval, minusTwoMinusOneInterval) 
      // case 2
      equalMulIntervals (minusThreeMinusTwoInterval, minusOneOneInterval) 
      // case 3
      equalMulIntervals (minusThreeMinusTwoInterval, zeroOneInterval) 
      equalMulIntervals (minusThreeMinusTwoInterval, fourFiveInterval) 
      // case 4
      equalMulIntervals (minusOneOneInterval, minusThreeMinusTwoInterval) 
      // case 5              
      equalMulIntervals (minusTwoTwoInterval, minusOneOneInterval) 
      // case 6              
      equalMulIntervals (minusTwoTwoInterval, fourFiveInterval) 
      // case 7              
      equalMulIntervals (fourFiveInterval, minusTwoTwoInterval) 
      // case 8              
      equalMulIntervals (fourFiveInterval, minusTwoTwoInterval) 
      // case 9              
      equalMulIntervals (twoThreeInterval, fourFiveInterval) 
    }
  }
}
newMulIntervalSpec execute
