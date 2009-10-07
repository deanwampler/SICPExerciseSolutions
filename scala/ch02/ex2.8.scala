type N = Double  // Shorthand
type Interval = Tuple2[N,N]

def makeInterval (a:N, b:N) = (a, b)
def lowerBound (x: Interval) = x._1
def upperBound (x: Interval) = x._2

def subInterval (x: Interval, y: Interval) =
  makeInterval (lowerBound(x) - upperBound(y), upperBound(x) - lowerBound(y))
                                 
val minusOneOneInterval = makeInterval (-1.0, 1.0)
val zeroOneInterval     = makeInterval ( 0.0, 1.0)
val twoThreeInterval    = makeInterval ( 2.0, 3.0)

val sub1 = subInterval (minusOneOneInterval, zeroOneInterval)
val sub2 = subInterval (minusOneOneInterval, twoThreeInterval)
val sub3 = subInterval (zeroOneInterval,     twoThreeInterval)

import org.scalatest._
import org.scalatest.matchers._

object subIntervalSpec extends Spec with ShouldMatchers {
  describe ("subInterval") {
    it ("""should return the diff of the lower-x and upper-y for the lower bound 
        and upper-x - lowery-y for the upper bound""") {
      lowerBound(sub1)  should equal (-2.0)
      upperBound(sub1)  should equal (1.0)
      lowerBound(sub2)  should equal (-4.0)
      upperBound(sub2)  should equal (-1.0)
      lowerBound(sub3)  should equal (-3.0)
      upperBound(sub3)  should equal (-1.0)
    }
  }
}
subIntervalSpec execute
