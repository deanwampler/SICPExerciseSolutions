type Vect = Tuple2[Double,Double]

def makeVect (x:Double, y:Double) = (x, y)
  
def xcorVect (v:Vect) = v._1

def ycorVect (v:Vect) = v._2

// If startPoint and endPoint are each (x,y) pairs, then they are already vectors
// as defined by make-vect above. We make a list of the points.

type Segment = Tuple2[Vect,Vect]

def makeSegment (startPoint: Vect, endPoint: Vect) = (startPoint, endPoint)

def startSegment (s: Segment) = s._1

def endSegment (s: Segment) = s._2
  
val zeroZero = makeVect(0, 0)
val threeTwo = makeVect(3, 2)
val seg      = makeSegment(zeroZero, threeTwo)

import org.scalatest._ 
import org.scalatest.matchers._

object segmentSpec extends Spec with ShouldMatchers {
  describe ("segment selectors") {
    it ("should return the components") {
      startSegment (seg) should equal (zeroZero)
      endSegment (seg) should equal (threeTwo)
    }
  }
}
segmentSpec execute