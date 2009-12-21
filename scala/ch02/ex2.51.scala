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
          
type Painter = (Frame) => Unit

def transformPainter (painter: Painter, origin: Vect, corner1: Vect, corner2: Vect) = {
  frame: Frame => 
  val m = frameCoordMap(frame)
  val newOrigin = m(origin)
  painter(makeFrame(newOrigin,
                    subVect(m(corner1), newOrigin),
                    subVect(m(corner2), newOrigin)))
}

def flipVert (painter: Painter) = 
  transformPainter(painter,
                   makeVect(0.0, 1.0),   // new origin
                   makeVect(1.0, 1.0),   // new end of edge1
                   makeVect(0.0, 0.0))   // new end of edge2

def flipHoriz (painter: Painter) = 
  transformPainter(painter,
                   makeVect(1.0, 0.0),   // new origin
                   makeVect(0.0, 0.0),   // new end of edge1
                   makeVect(1.0, 1.0))   // new end of edge2

def rotateCounterclockwise180 (painter: Painter) = 
  transformPainter(painter,
                   makeVect(1.0, 1.0),   // new origin
                   makeVect(0.0, 1.0),   // new end of edge1
                   makeVect(1.0, 0.0))   // new end of edge2

def rotateCounterclockwise270 (painter: Painter) = 
  transformPainter(painter,
                   makeVect(0.0, 1.0),   // new origin
                   makeVect(0.0, 0.0),   // new end of edge1
                   makeVect(1.0, 1.0))   // new end of edge2

def beside (painter1: Painter, painter2: Painter) = {
  val splitPoint = makeVect(0.5, 0.0)
  val paintLeft = transformPainter (painter1, 
                                    makeVect(0.0, 0.0), 
                                    splitPoint, 
                                    makeVect(0.0, 1.0))
  val paintRight = transformPainter (painter2, 
                                    splitPoint, 
                                    makeVect(1.0, 0.0), 
                                    makeVect(0.5, 1.0))
  (frame: Frame) => 
    paintLeft(frame)
    paintRight(frame)
}

def below1 (painter1: Painter, painter2: Painter) = {
  val splitPoint = makeVect(0.0, 0.5)
  val paintBotton = transformPainter (painter1, 
                                    makeVect(0.0, 0.0), 
                                    makeVect(1.0, 0.0), 
                                    splitPoint)
  val paintTop = transformPainter (painter2, 
                                    splitPoint, 
                                    makeVect(1.0, 0.5), 
                                    makeVect(0.0, 1.0))
  (frame: Frame) => 
    paintBotton(frame)
    paintTop(frame)
}

def below2 (painter1: Painter, painter2: Painter) =
  rotateCounterclockwise270(
    rotateCounterclockwise180(
      beside(rotateCounterclockwise270(painter1), rotateCounterclockwise270(painter2))))

// triangle with points at (0, 0), (1, .5) and (.5, 1).
def triangle1Painter (frame: Frame) = {
  val zeroZeroToOnePointFive = 
        makeSegment(makeVect(0.0, 0.0), makeVect(1.0, 0.5))
  val onePointFiveToPointFiveOne =
        makeSegment(makeVect(1.0, 0.5), makeVect(0.5, 1.0))
  val pointFiveOneToZeroZero =
        makeSegment(makeVect(0.5, 1.0), makeVect(0.0, 0.0))
  segmentsPainter(List(zeroZeroToOnePointFive,
                       onePointFiveToPointFiveOne,
                       pointFiveOneToZeroZero)) (frame)
}

// triangle with points at (0, 1), (1, .5) and (.5, 0), which is triangle1 
// flipped vertically.
def triangle1VertPainter (frame: Frame) = {
  val zeroOneToOnePointFive = 
        makeSegment(makeVect(0.0, 1.0), makeVect(1.0, 0.5))
  val onePointFiveToPointFiveZero =
        makeSegment(makeVect(1.0, 0.5), makeVect(0.5, 0.0))
  val pointFiveZeroToZeroOne =
        makeSegment(makeVect(0.5, 0.0), makeVect(0.0, 1.0))
  segmentsPainter(List(zeroOneToOnePointFive,
                       onePointFiveToPointFiveZero,
                       pointFiveZeroToZeroOne)) (frame)
}


// triangle with points at (1, 0), (0, .5) and (.5, 1), which is triangle1 
// flipped horizontally.
def triangle1HorizPainter (frame: Frame) = {
  val oneZeroToZeroPointFive = 
        makeSegment(makeVect(1.0, 0.0), makeVect(0.0, 0.5))
  val zeroPointFiveToPointFiveOne =
        makeSegment(makeVect(0.0, 0.5), makeVect(0.5, 1.0))
  val pointFiveOneToOneZero =
        makeSegment(makeVect(0.5, 1.0), makeVect(1.0, 0.0))
  segmentsPainter(List(oneZeroToZeroPointFive,
                       zeroPointFiveToPointFiveOne,
                       pointFiveOneToOneZero)) (frame)
}

val origin = makeVect(0.0, 0.0)
val edge1  = makeVect(1.0, 0.0) // edges are not relative to the origin values.
val edge2  = makeVect(0.0, 1.0)
val frame  = makeFrame(origin, edge1, edge2)

import org.scalatest._ 
import org.scalatest.matchers._

object belowPainterSpec extends Spec with ShouldMatchers {
  describe ("below1 and below2") {
    it ("should orient painter1 below painter2") {
      below1(triangle1Painter, triangle1VertPainter)(frame) 
      val result1 = output
      resetOutput
      below2(triangle1Painter, triangle1VertPainter)(frame) 
      result1 should equal (output)
      resetOutput

      below1(triangle1Painter, triangle1HorizPainter)(frame) 
      val result2 = output
      resetOutput
      below2(triangle1Painter, triangle1HorizPainter)(frame) 
      result2 should equal (output)
      resetOutput
    }
  }
}
belowPainterSpec execute
