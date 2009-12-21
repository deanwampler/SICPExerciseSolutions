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

def scaleVect (factor:Double, v:Vect) =
  makeVect (factor * xcorVect(v),
            factor * ycorVect(v))

// If startPoint and endPoint are each (x,y) pairs, then they are already vectors
// as defined by makeVect above. We make a list of the points.

type Segment = Tuple2[Vect,Vect]

def makeSegment (startPoint: Vect, endPoint: Vect) = (startPoint, endPoint)

def startSegment (s: Segment) = s._1

def endSegment (s: Segment) = s._2
  
type Frame = Tuple3[Vect,Vect,Vect]

def makeFrame (origin:Vect, edge1:Vect, edge2:Vect) = 
  (origin, edge1, edge2)

def originFrame (frame:Frame) = frame._1

def edge1Frame (frame:Frame) = frame._2

def edge2Frame (frame:Frame) = frame._3

// For the exercise, drawLine appens the pair of the line ends to a string.
var output = ""
def drawLine (start: Vect, end: Vect) = output += "(" + start + "," + end + ")"
def resetOutput = output = ""

def frameCoordMap (frame: Frame) = (v: Vect) =>
  addVect(
    originFrame(frame),
    addVect (scaleVect (xcorVect(v), edge1Frame(frame)),
             scaleVect (ycorVect(v), edge2Frame(frame))))
                            
def segmentsPainter (segments: List[Segment]) = (frame: Frame) =>
  segments foreach { segment => 
    drawLine (frameCoordMap(frame) (startSegment(segment)),
              frameCoordMap(frame) (endSegment(segment)))
  }
          
def outlinePainter (frame: Frame) = {
  val zeroZeroToOneZero = makeSegment (makeVect(0, 0), makeVect(1, 0))
  val oneZeroToOneOne   = makeSegment (makeVect(1, 0), makeVect(1, 1))
  val oneOneToZeroOne   = makeSegment (makeVect(1, 1), makeVect(0, 1))
  val zeroOneToZeroZero = makeSegment (makeVect(0, 1), makeVect(0, 0))
  segmentsPainter (zeroZeroToOneZero :: oneZeroToOneOne :: 
                   oneOneToZeroOne :: zeroOneToZeroZero :: Nil) (frame)
}
                            
val origin1 = makeVect(0, 0)
val edge11  = makeVect(1, 0) // edges are not relative to the origin values.
val edge21  = makeVect(0, 1)

val origin2 = makeVect(-1, 1)
val edge12  = makeVect( 2, 1)
val edge22  = makeVect(-1, 1)

import org.scalatest._ 
import org.scalatest.matchers._

object outlinePainterSpec extends Spec with ShouldMatchers {
  describe ("outlinePainter") {
    it ("should trace the corners of the frame") {
      outlinePainter (makeFrame (origin1, edge11, edge21))
      output should equal ("((0.0,0.0),(1.0,0.0))((1.0,0.0),(1.0,1.0))((1.0,1.0),(0.0,1.0))((0.0,1.0),(0.0,0.0))")
      resetOutput
      outlinePainter (makeFrame (origin2, edge11, edge21))
      output should equal ("((-1.0,1.0),(0.0,1.0))((0.0,1.0),(0.0,2.0))((0.0,2.0),(-1.0,2.0))((-1.0,2.0),(-1.0,1.0))")
      resetOutput
      outlinePainter (makeFrame (origin1, edge12, edge22))
      output should equal ("((0.0,0.0),(2.0,1.0))((2.0,1.0),(1.0,2.0))((1.0,2.0),(-1.0,1.0))((-1.0,1.0),(0.0,0.0))")
      resetOutput
      outlinePainter (makeFrame (origin2, edge12, edge22))
      output should equal ("((-1.0,1.0),(1.0,2.0))((1.0,2.0),(0.0,3.0))((0.0,3.0),(-2.0,2.0))((-2.0,2.0),(-1.0,1.0))")
      resetOutput
    }
  }
}
outlinePainterSpec execute

def xPainter (frame: Frame) = {
  val zeroZeroToOneOne = makeSegment (makeVect(0, 0), makeVect(1, 1))
  val oneZeroToZeroOne = makeSegment (makeVect(1, 0), makeVect(0, 1))
  segmentsPainter (zeroZeroToOneOne :: oneZeroToZeroOne :: Nil) (frame)
}
                            
object xPainterSpec extends Spec with ShouldMatchers {
  describe ("xPainter") {
    it ("should trace an 'x' between the corners of the frame") {
      xPainter (makeFrame (origin1, edge11, edge21))
      output should equal ("((0.0,0.0),(1.0,1.0))((1.0,0.0),(0.0,1.0))")
      resetOutput
      xPainter (makeFrame (origin2, edge11, edge21))
      output should equal ("((-1.0,1.0),(0.0,2.0))((0.0,1.0),(-1.0,2.0))")
      resetOutput
      xPainter (makeFrame (origin1, edge12, edge22))
      output should equal ("((0.0,0.0),(1.0,2.0))((2.0,1.0),(-1.0,1.0))")
      resetOutput
      xPainter (makeFrame (origin2, edge12, edge22))
      output should equal ("((-1.0,1.0),(0.0,3.0))((1.0,2.0),(-2.0,2.0))")
      resetOutput
    }
  }
}
xPainterSpec execute

def diamondPainter (frame: Frame) = {
  val one   = makeSegment (makeVect(0.5, 0), makeVect(1, 0.5))
  val two   = makeSegment (makeVect(1, 0.5), makeVect(0.5, 1))
  val three = makeSegment (makeVect(0.5, 1), makeVect(0, 0.5))
  val four  = makeSegment (makeVect(0, 0.5), makeVect(0.5, 0))
  segmentsPainter (one :: two :: three :: four :: Nil) (frame)
}
                            
object diamondPainterSpec extends Spec with ShouldMatchers {
  describe ("diamondPainter") {
    it ("should trace a diamond in the frame") {
      diamondPainter (makeFrame (origin1, edge11, edge21))
      output should equal ("((0.5,0.0),(1.0,0.5))((1.0,0.5),(0.5,1.0))((0.5,1.0),(0.0,0.5))((0.0,0.5),(0.5,0.0))")
      resetOutput
      diamondPainter (makeFrame (origin2, edge11, edge21))
      output should equal ("((-0.5,1.0),(0.0,1.5))((0.0,1.5),(-0.5,2.0))((-0.5,2.0),(-1.0,1.5))((-1.0,1.5),(-0.5,1.0))")
      resetOutput
      diamondPainter (makeFrame (origin1, edge12, edge22))
      output should equal ("((1.0,0.5),(1.5,1.5))((1.5,1.5),(0.0,1.5))((0.0,1.5),(-0.5,0.5))((-0.5,0.5),(1.0,0.5))")
      resetOutput
      diamondPainter (makeFrame (origin2, edge12, edge22))
      output should equal ("((0.0,1.5),(0.5,2.5))((0.5,2.5),(-1.0,2.5))((-1.0,2.5),(-1.5,1.5))((-1.5,1.5),(0.0,1.5))")
      resetOutput
    }
  }
}
diamondPainterSpec execute

// skipped d) the wave painter.
