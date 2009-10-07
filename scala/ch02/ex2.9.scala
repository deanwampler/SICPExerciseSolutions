type N = Double  // Shorthand
type Interval = Tuple2[N,N]

def makeInterval (a:N, b:N) = (a, b)
def lowerBound (x: Interval) = x._1
def upperBound (x: Interval) = x._2

def width (x: Interval) = (lowerBound(x) + upperBound(x)) / 2

def addInterval (x: Interval, y: Interval) =
  makeInterval (lowerBound(x) + lowerBound(y), upperBound(x) + upperBound(y))

def subInterval (x: Interval, y: Interval) =
  makeInterval (lowerBound(x) - upperBound(y), upperBound(x) - lowerBound(y))
                                 
val minusOneOneInterval = makeInterval (-1.0, 1.0)
val zeroOneInterval     = makeInterval ( 0.0, 1.0)
val twoThreeInterval    = makeInterval ( 2.0, 3.0)
val fourFiveInterval    = makeInterval ( 4.0, 5.0)

import org.scalatest._
import org.scalatest.matchers._

object widthAddSubIntervalSpec extends Spec with ShouldMatchers {
  describe ("subInterval") {
    it ("""should return the diff of the lower-x and upper-y for the lower bound 
        and upper-x - lowery-y for the upper bound""") {
      width (addInterval (minusOneOneInterval, minusOneOneInterval)) should 
        equal (width (minusOneOneInterval) + width (minusOneOneInterval))
      width (addInterval (minusOneOneInterval, zeroOneInterval)) should 
        equal (width (minusOneOneInterval) + width (zeroOneInterval))
      width (addInterval (minusOneOneInterval, fourFiveInterval)) should 
        equal (width (minusOneOneInterval) + width (fourFiveInterval))
      width (addInterval (zeroOneInterval, twoThreeInterval)) should 
        equal (width (zeroOneInterval) + width (twoThreeInterval))
      width (addInterval (twoThreeInterval, fourFiveInterval)) should 
        equal (width (twoThreeInterval) + width (fourFiveInterval))
    }
  }
}
widthAddSubIntervalSpec execute

