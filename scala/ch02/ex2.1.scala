def gcd (a: Int, b: Int): Int = b match {
  case 0 => a
  case _ => gcd(b, a % b)
}
      
def abs (n: Int) = math.abs(n)

def rationalizeSigns (n: Int, d: Int) = 
  if (n * d < 0) (- abs(n), abs(d)) else (abs(n), abs(d))

type rat = Pair[Int, Int]

def makeRat (n: Int, d: Int) = {
  val nd = rationalizeSigns (n, d)
  val g  = gcd (abs(nd._1), abs(nd._2))
  (nd._1 / g, nd._2 / g)
}
    
def numer (x: rat) = x._1
def denom (x: rat) = x._2

def displayRat (r: rat) = format("%d/%d", numer(r), denom(r))
  
def addRat (x: rat, y: rat) = 
  makeRat (numer(x) * denom(y) + numer(y) * denom(x), denom(x) * denom(y)) 

def subRat (x: rat, y: rat) = 
  makeRat (numer(x) * denom(y) - numer(y) * denom(x), denom(x) * denom(y)) 

def mulRat (x: rat, y: rat) = 
  makeRat (numer(x) * numer(y), denom(x) * denom(y)) 

def divRat (x: rat, y: rat) = 
  makeRat (numer(x) * denom(y), denom(x) * numer(y)) 

def equalRat (x: rat, y: rat) = numer(x) * denom(y) == numer(y) * denom(x)


val oneHalf     = makeRat(1, 2)
val oneThird    = makeRat(1, 3)
val fiveSixths  = addRat(oneHalf,  oneThird)
val oneSixthSub = subRat(oneHalf,  oneThird)
val oneSixthMul = mulRat(oneHalf,  oneThird)
val threeHalves = divRat(oneHalf,  oneThird)
val twoThirds   = addRat(oneThird, oneThird)

val minusMinusOneHalf    = makeRat(1,2)
val minusOneHalf         = makeRat(-1,2)
val minusOneThird        = makeRat(1, -3)
val minusOneSixthAddA    = addRat(minusOneHalf,  oneThird)
val plusOneSixthAddB     = addRat(oneHalf,       minusOneThird)
val minusFiveSixthsSubA  = subRat(minusOneHalf,  oneThird)
val plusFiveSixthsSubB   = subRat(oneHalf,       minusOneThird)
val minusOneSixthMulA    = mulRat(minusOneHalf,  oneThird)
val minusOneSixthMulB    = mulRat(oneHalf,       minusOneThird)
val minusThreeHalvesA    = divRat(minusOneHalf,  oneThird)
val minusThreeHalvesB    = divRat(oneHalf,       minusOneThird)
val minusTwoThirdsA      = addRat(minusOneThird, minusOneThird)
val zeroA                = addRat(minusOneThird, oneThird)
val zeroB                = addRat(oneThird,      minusOneThird)

import org.scalatest._
import org.scalatest.matchers._

object rationalSpec extends Spec with ShouldMatchers {
  def checkRatEqual (r: rat, expectedN: Int, expectedD: Int) = { 
    displayRat(r)
    print(": ")
    numer(r) should equal (expectedN)
    denom(r) should equal (expectedD)
  }
  
  describe ("rationalizeSigns") {
    it ("should return a tuple with (-|n|, |d|) if n*d < 0") {
      rationalizeSigns(-1, 2) should equal (-1, 2)
      rationalizeSigns(1, -2) should equal (-1, 2)
    }
    it ("should return a tuple with (|n|, |d|) if n < 0 and d < 0") {
      rationalizeSigns(-1, -2) should equal (1, 2)
      rationalizeSigns(-1, -2) should equal (1, 2)
    }
    it ("should return a tuple with (n, d) if n >= 0 and d >= 0") {
      rationalizeSigns(0, 0) should equal (0, 0)
      rationalizeSigns(1, 2) should equal (1, 2)
      rationalizeSigns(1, 2) should equal (1, 2)
    }
  }

  describe ("makeRat") {
    it ("should properly initialize a rational number") {
      checkRatEqual (oneHalf,     1, 2)
      checkRatEqual (oneThird,    1, 3)
      checkRatEqual (fiveSixths,  5, 6)
      checkRatEqual (oneSixthSub, 1, 6)
      checkRatEqual (oneSixthMul, 1, 6)
      checkRatEqual (threeHalves, 3, 2)
      checkRatEqual (twoThirds,   2, 3)
      checkRatEqual (minusMinusOneHalf, 1, 2)
      checkRatEqual (minusOneHalf,  -1, 2)
      checkRatEqual (minusOneThird, -1, 3)  
    }
  }
  describe ("equalRat") {
    it ("should correctly determine if two rational numbers are equal") {
      equalRat(oneHalf, oneHalf)  should be (true)
      equalRat(oneHalf, oneThird) should be (false)
      equalRat(oneSixthSub, oneSixthMul) should be (true)
    }
  }

  describe ("makeRat") {
    it ("should properly handle negative numerators and/or denominators") {
      checkRatEqual (minusOneSixthAddA,    -1, 6)
      checkRatEqual (plusOneSixthAddB,      1, 6)
      checkRatEqual (minusFiveSixthsSubA,  -5, 6)
      checkRatEqual (plusFiveSixthsSubB,    5, 6)
      checkRatEqual (minusOneSixthMulA,    -1, 6)
      checkRatEqual (minusOneSixthMulB,    -1, 6)
      checkRatEqual (minusThreeHalvesA,    -3, 2)
      checkRatEqual (minusThreeHalvesB,    -3, 2)
      checkRatEqual (minusTwoThirdsA,      -2, 3)
      checkRatEqual (zeroA,                 0, 1)
      checkRatEqual (zeroB,                 0, 1)
    }
  }
}
rationalSpec execute
