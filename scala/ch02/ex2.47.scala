type Vect = Tuple2[Double,Double]

def makeVect (x:Double, y:Double) = (x, y)
  
def xcorVect (v:Vect) = v._1

def ycorVect (v:Vect) = v._2

type Frame1 = Tuple3[Vect,Vect,Vect]

def makeFrame1 (origin:Vect, edge1:Vect, edge2:Vect) = 
  (origin, edge1, edge2)

def originFrame1 (frame:Frame1) = frame._1

def edge1Frame1 (frame:Frame1) = frame._2

def edge2Frame1 (frame:Frame1) = frame._3

type Frame2 = Tuple2[Vect,Tuple2[Vect,Vect]]

def makeFrame2 (origin:Vect, edge1:Vect, edge2:Vect) = 
  (origin, (edge1, edge2))

def originFrame2 (frame:Frame2) = frame._1

def edge1Frame2 (frame:Frame2) = frame._2._1

def edge2Frame2 (frame:Frame2) = frame._2._2

val o  = makeVect(1, 2)
val e1 = makeVect(2, 3)
val e2 = makeVect(0, 4)
val f1 = makeFrame1(o, e1, e2)
val f2 = makeFrame2(o, e1, e2)

import org.scalatest._ 
import org.scalatest.matchers._

object frameSpec extends Spec with ShouldMatchers {
  describe ("frame selectors - 1st implementation") {
    it ("should return the components") {
      originFrame1(f1) should equal (o)
      edge1Frame1(f1)  should equal (e1)
      edge2Frame1(f1)  should equal (e2)
    }
  }
  describe ("frame selectors - 2nd implementation") {
    it ("should return the components") {
      originFrame2(f2) should equal (o)
      edge1Frame2(f2)  should equal (e1)
      edge2Frame2(f2)  should equal (e2)
    }
  }
}

frameSpec execute