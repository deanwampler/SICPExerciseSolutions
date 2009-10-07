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
    
def divInterval (x: Interval, y: Interval) = {
  if (lowerBound(y) == 0.0 || upperBound(y) == 0.0)
    throw new RuntimeException("Divide by zero for y = "+y)
  mulInterval (x, makeInterval (1.0 / upperBound(y), 1.0 / lowerBound(y))) 
}
                                 
import org.scalatest._
import org.scalatest.matchers._

object divIntervalSpec extends Spec with ShouldMatchers {
  describe ("divInterval") {
    it ("should throw an exception on divide by zero error") {
      intercept[RuntimeException] {
        divInterval(makeInterval(1.0,1.0), makeInterval(0.0,2.0))
      }
      intercept[RuntimeException] {
        divInterval(makeInterval(1.0,1.0), makeInterval(2.0,0.0))
      }
    }
  }
}
divIntervalSpec execute

