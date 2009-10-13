type N = Double  // Shorthand
type Interval = Tuple2[N,N]

def makeInterval (a:N, b:N) = (a, b)
def lowerBound (x: Interval) = x._1
def upperBound (x: Interval) = x._2

def makeCenterPercent (center: N, percent: N) = {
  val halfDelta = center * (percent / 100.0)
  makeInterval (center - halfDelta, center + halfDelta)
}

def center (i: Interval) = (lowerBound(i) + upperBound(i)) / 2.0
  
def percent (i: Interval) = {
  val halfDelta = (upperBound(i) - lowerBound(i)) / 2.0
  halfDelta * (lowerBound(i) + halfDelta) / 100.0
}

val i = makeCenterPercent(100.0, 1.0)

import org.scalatest._
import org.scalatest.matchers._

object percentIntervalSpec extends Spec with ShouldMatchers {
  describe ("makeCenterPercent") {
    it ("should make an interval using center and percent values") {
      lowerBound(i) should equal (99.0)
      upperBound(i) should equal (101.0)
      center(i)     should equal (100.0)
      percent(i)    should equal (1.0)      
    }
  }
}
percentIntervalSpec execute