type Vect = Tuple2[Double,Double]

def makeVect (x:Double, y:Double) = (x, y)
  
def xcorVect (v:Vect) = v._1

def ycorVect (v:Vect) = v._2
  
def addVect (v1:Vect, v2:Vect) =
  makeVect (xcorVect(v1) + xcorVect(v2),
            ycorVect(v1) + ycorVect(v2))

def subVect (v1:Vect, v2:Vect) =
  makeVect (xcorVect(v1) - xcorVect(v2),
            ycorVect(v1) - ycorVect(v2))

def scaleVect (v:Vect, factor:Double) =
  makeVect (factor * xcorVect(v),
            factor * ycorVect(v))

val v1 = makeVect(2, 3)
val v2 = makeVect(5, 6)

import org.scalatest._ 
import org.scalatest.matchers._

object vectorSpec extends Spec with ShouldMatchers {
  describe ("addVect") {
    it ("should return a new vector with the x and y coordinates added") {
      xcorVect(addVect(v1, v2)) should equal (7.)
      ycorVect(addVect(v1, v2)) should equal (9.)      
    }
  }
  describe ("subVect") {
    it ("should return a new vector with the x and y coordinates subtracted") {
      xcorVect(subVect(v1, v2)) should equal (-3.)
      ycorVect(subVect(v1, v2)) should equal (-3.)      
    }
  }
  describe ("scalaVect") {
    it ("should return a new vector with the x and y coordinates scaled by the input factor") {
      xcorVect(scaleVect(v1, 3)) should equal (6.)
      ycorVect(scaleVect(v1, 3)) should equal (9.)      
    }
  }
}  
vectorSpec execute
