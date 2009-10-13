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

  if (bounds(<, <, <, <))
    mkInterval (upperBound(x), upperBound(y) lowerBound(x) lowerBound(y))
  else if (bounds (<, <, <, >=))
    mkInterval (lowerBound(x) upperBound(y) lowerBound(x) lowerBound(y))
  else if (bounds (<, <, >=, >=))
    mkInterval (lowerBound(x) upperBound(y) upperBound(x) lowerBound(y))
  else if (bounds (<, >=, <, <))
    mkInterval (upperBound(x) lowerBound(y) lowerBound(x) lowerBound(y))
  else if (bounds (<, >=, <, >=)) {
    val p1 = lowerBound(x) * upperBound(y)
    val p2 = upperBound(x) * lowerBound(y)
    val p3 = lowerBound(x) * lowerBound(y)
    val p4 = upperBound(x) * upperBound(y)
    makeInterval (min (p1, p1), max (p3, p4))
  }
  else if (bounds (<, >=, >=, >=))
    mkInterval (lowerBound(x) upperBound(y) upperBound(x) upperBound(y))
  else if (bounds (>=, >=, <, <))
    mkInterval (upperBound(x) upperBound(y) upperBound(x) lowerBound(y))
  else if (bounds (>=, >=, <, >=))
    mkInterval (upperBound(x) lowerBound(y) upperBound(x) upperBound(y))
  else   
    mkInterval (lowerBound(x) lowerBound(y) upperBound(x) upperBound(y))
}

val minus-three-minus-two-interval = makeInterval (-3.0, -2.0)
val minus-two-minus-one-interval = makeInterval (-2.0, -1.0)
val minus-two-two-interval = makeInterval (-2.0, 2.0)
val minus-one-one-interval = makeInterval (-1.0, 1.0)
val zero-one-interval      = makeInterval ( 0.0, 1.0)
val two-three-interval     = makeInterval ( 2.0, 3.0)
val four-five-interval     = makeInterval ( 4.0, 5.0)

import org.scalatest._
import org.scalatest.matchers._

object newMulIntervalSpec extends Spec with ShouldMatchers {
  def equalMulIntervals (x: Interval, y: Interval) = {
    val expected = mulIntervalOld (x, y)
    val actual   = mulInterval (x, y)
    lowerBound(actual) should equal lowerBound(expected)
    upperBound(actual) should equal upperBound(expected)
  }
  describe ("new mulInterval") {
    it ("should compute the same result as the old mulInterval") {
      // case 1
      equalMulIntervals (minus-three-minus-two-interval, minus-two-minus-one-interval) 
      // case 2
      equalMulIntervals (minus-three-minus-two-interval, minus-one-one-interval) 
      // case 3
      equalMulIntervals (minus-three-minus-two-interval, zero-one-interval) 
      equalMulIntervals (minus-three-minus-two-interval, four-five-interval) 
      // case 4
      equalMulIntervals (minus-one-one-interval, minus-three-minus-two-interval) 
      // case 5              
      equalMulIntervals (minus-two-two-interval, minus-one-one-interval) 
      // case 6              
      equalMulIntervals (minus-two-two-interval, four-five-interval) 
      // case 7              
      equalMulIntervals (four-five-interval, minus-two-two-interval) 
      // case 8              
      equalMulIntervals (four-five-interval, minus-two-two-interval) 
      // case 9              
      equalMulIntervals (two-three-interval, four-five-interval) 
    }
  }
}
newMulIntervalSpec execute
