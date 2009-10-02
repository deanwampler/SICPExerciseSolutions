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

val zeroZero = makePoint (0.0, 0.0)
val zeroTwo  = makePoint (0.0, 2.0)
val twoZero  = makePoint (2.0, 0.0)
val zeroOne  = makePoint (0.0, 1.0)
val oneZero  = makePoint (1.0, 0.0)
val oneOne   = makePoint (1.0, 1.0)
val twoTwo   = makePoint (2.0, 2.0)
val mOneMOne = makePoint (-1.0, -1.0)
val mTwoMTwo = makePoint (-2.0, -2.0)

val zeroZeroZeroZero = makeSegment (zeroZero, zeroZero)
val zeroZeroTwoTwo   = makeSegment (zeroZero, twoTwo)
val zeroZeroMOneMOne = makeSegment (zeroZero, mOneMOne)
val zeroZeroMTwoMTwo = makeSegment (zeroZero, mTwoMTwo)
val zeroZeroTwoZero  = makeSegment (zeroZero, twoZero)
val zeroZeroZeroTwo  = makeSegment (zeroZero, zeroTwo)
val twoTwoZeroZero   = makeSegment (twoTwo, zeroZero)
val mTwoMTwoZeroZero = makeSegment (mTwoMTwo, zeroZero)
val twoZeroZeroZero  = makeSegment (twoZero, zeroZero)
val zeroTwoZeroZero  = makeSegment (zeroTwo, zeroZero)
val mTwoMTwoTwoTwo   = makeSegment (mTwoMTwo, twoTwo)
val twoTwoMTwoMTwo   = makeSegment (twoTwo, mTwoMTwo)

val midZeroZeroA = midpointSegment (zeroZeroZeroZero)
val midZeroZeroB = midpointSegment (mTwoMTwoTwoTwo)
val midZeroZeroC = midpointSegment (twoTwoMTwoMTwo)
val midOneOne    = midpointSegment (zeroZeroTwoTwo)
val midOneZeroA  = midpointSegment (zeroZeroTwoZero)
val midOneZeroB  = midpointSegment (twoZeroZeroZero)
val midZeroOneA  = midpointSegment (zeroZeroZeroTwo)
val midZeroOneB  = midpointSegment (zeroTwoZeroZero)

import org.scalatest._
import org.scalatest.matchers._

object segmentsSpec extends Spec with ShouldMatchers {
  def isEqualPoint(p :point, expected: point) = {
    xPoint(p) should equal (xPoint(expected))
    yPoint(p) should equal (yPoint(expected))
  }
  
  describe ("midpoint calculation of Segments") {
    it ("should find the point in the middle") {
      isEqualPoint (midZeroZeroA, zeroZero)
      isEqualPoint (midZeroZeroB, zeroZero)
      isEqualPoint (midZeroZeroC, zeroZero)
      isEqualPoint (midOneOne,    oneOne)
      isEqualPoint (midOneZeroA,  oneZero)
      isEqualPoint (midOneZeroB,  oneZero)
      isEqualPoint (midZeroOneA,  zeroOne)
      isEqualPoint (midZeroOneB,  zeroOne)
    }
  }
}
segmentsSpec execute

printPoint (midZeroZeroA)
printPoint (midZeroZeroB)
printPoint (midZeroZeroC)
printPoint (midOneOne)
printPoint (midOneZeroA)
printPoint (midOneZeroB)
printPoint (midZeroOneA)
printPoint (midZeroOneB)
println("")
printPoint (zeroZero)
printPoint (oneZero)
printPoint (zeroOne)
printPoint (twoZero)
printPoint (zeroTwo)
printPoint (oneOne)
printPoint (twoTwo)
printPoint (mOneMOne)
printPoint (mTwoMTwo)
println("")
