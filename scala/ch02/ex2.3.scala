// Points and segments from ex. 2.2

type point = Tuple2[Double,Double]

def makePoint (x: Double, y: Double) = (x, y)

def xPoint (p: point) = p._1
def yPoint (p: point) = p._2
  
type segment = Tuple2[point,point]

def makeSegment(start: point, end: point) = (start, end)
  
def startSegment (s: segment) = s._1
def endSegment   (s: segment) = s._2

def midpointSegment (s: segment) = {
  val x1 = xPoint (startSegment(s))
  val y1 = yPoint (startSegment(s))
  val x2 = xPoint (endSegment(s))
  val y2 = yPoint (endSegment(s))
  makePoint ((x1 + x2) / 2, (y1 + y2) / 2)
}

def printPoint (p: point) = format ("(%f,%f)\n", xPoint(p), yPoint(p))

def equalPoints (p1: point, p2: point) = 
    xPoint(p1) == xPoint(p2) && yPoint(p1) == yPoint(p2)

// Sqrt from ex 1.46

val tolerance = 0.00001

def square(x: Double) = x*x

def iterativeImprove(goodEnough: (Double) => Boolean, improve: (Double) => Double) = {
  def iterate(guess2: Double): Double = goodEnough(guess2) match {
    case true => guess2
    case _ => iterate(improve(guess2))
  }
  (guess:Double) => iterate(guess)
}

def average(x: Double, y: Double) = (x + y) / 2.0

def goodEnough(guess: Double, x: Double) = math.abs(square(guess) - x) < tolerance

def improve(guess: Double, x: Double) = average(guess, x / guess)
  
def sqrt(x: Double) = iterativeImprove(
  (guess:Double) => goodEnough(guess, x), 
  (guess:Double) => improve(guess, x))(1.0)
    
// Test rectangle implementations.

val zeroZero = makePoint (0.0, 0.0)
val zeroTwo  = makePoint (0.0, 2.0)
val twoZero  = makePoint (2.0, 0.0)
val oneOne   = makePoint (1.0, 1.0)
val twoTwo   = makePoint (2.0, 2.0)
val mOneOne  = makePoint (-1.0, 1.0)

import org.scalatest._
import org.scalatest.matchers._

// Generic line length calculator:

def lengthLine (p1: point, p2: point) = sqrt(
    square (xPoint(p2) - xPoint(p1)) + square (yPoint(p2) - yPoint(p1)))

// Rectangles: an implementation that uses 4 points.

type rect = Tuple4[point,point,point,point]

// counterclockwise - but doesn't assume horizontal-vertical
// p4 -- p3
// |     |
// p1 -- p2 
def makeRect (p1: point, p2: point, p3: point, p4: point) = (p1, p2, p3, p4)

def heightRect (r: rect) = lengthLine (r._4, r._1)

def widthRect (r: rect) = lengthLine (r._2, r._1)

// pass in the functions to calculate the height and width, so we can configure
// them using different rectangle implementations.

def areaRect[R] (heightCalc: (R) => Double, widthCalc: (R) => Double, r: R) =
  heightCalc(r) * widthCalc(r)

def perimeterRect[R] (heightCalc: (R) => Double, widthCalc: (R) => Double, r: R) =
  2 * (heightCalc(r) + widthCalc(r))

val sqrtTwo = sqrt(2.0)

import org.scalatest._
import org.scalatest.matchers._

object rectWith4PointsSpec extends Spec with ShouldMatchers {
  describe ("Rectangle represented by 4 points") {
    it ("should calculate the area and perimeter") {
      val rect1 = makeRect (zeroZero, twoZero, twoTwo,  zeroTwo)
      widthRect     (rect1) should be (2.0 plusOrMinus tolerance)
      heightRect    (rect1) should be (2.0 plusOrMinus tolerance)
      perimeterRect (heightRect, widthRect, rect1) should be (8.0 plusOrMinus tolerance)
      areaRect      (heightRect, widthRect, rect1) should be (4.0 plusOrMinus tolerance)

      val rect2 = makeRect (zeroZero, oneOne,  zeroTwo, mOneOne)
      widthRect     (rect2) should be (sqrtTwo plusOrMinus tolerance)
      heightRect    (rect2) should be (sqrtTwo plusOrMinus tolerance)
      perimeterRect (heightRect, widthRect, rect2) should be ((4.0*sqrtTwo) plusOrMinus tolerance)
      areaRect      (heightRect, widthRect, rect2) should be (2.0 plusOrMinus tolerance)
    }
  }
}
rectWith4PointsSpec execute

// Rectangles: 2nd implementation that uses 2 segments. 1st is assumed to be the
// more vertical; its length is taken to be the height. The 2nd is assumed to be
// the more horizontal; its length is the width.
// Area and perimeter methods unchanged.

type rectSegment = Tuple2[segment,segment]

// start point for both segments must be the same.
def makeRectSegments (s1: segment, s2: segment) = {
  if (equalPoints (startSegment(s1), startSegment(s2)) == false) 
    throw new RuntimeException ("must use segments with the same starting points.")
  else
    (s1, s2)
}

def heightRectSegment (r: rectSegment) = {
  val p2 = startSegment (r._1)
  val p1 = endSegment   (r._1)
  lengthLine (p1, p2)
}

def widthRectSegment (r: rectSegment) = {
  val p2 = startSegment (r._2)
  val p1 = endSegment   (r._2)
  lengthLine (p1, p2)
}

object rectWith2SegmentsSpec extends Spec with ShouldMatchers {
  describe ("Rectangle represented by 2 segments") {
    it ("should calculate the area and perimeter") {
      val rect3 = makeRectSegments (makeSegment(zeroZero, twoZero), makeSegment(zeroZero, zeroTwo))
      widthRectSegment  (rect3) should be (2.0 plusOrMinus tolerance)
      heightRectSegment (rect3) should be (2.0 plusOrMinus tolerance)
      perimeterRect     (heightRectSegment, widthRectSegment, rect3) should be (8.0 plusOrMinus tolerance)
      areaRect          (heightRectSegment, widthRectSegment, rect3) should be (4.0 plusOrMinus tolerance)

      val rect4 = makeRectSegments (makeSegment(zeroZero, oneOne), makeSegment(zeroZero, mOneOne))
      widthRectSegment  (rect4) should be (sqrtTwo plusOrMinus tolerance)
      heightRectSegment (rect4) should be (sqrtTwo plusOrMinus tolerance)
      perimeterRect     (heightRectSegment, widthRectSegment, rect4) should be ((4.0*sqrtTwo) plusOrMinus tolerance)
      areaRect          (heightRectSegment, widthRectSegment, rect4) should be (2.0 plusOrMinus tolerance)
    }
  }
}
rectWith2SegmentsSpec execute
