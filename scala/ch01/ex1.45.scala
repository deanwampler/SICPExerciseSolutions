val tolerance = 0.00001

def closeEnough(x: Double, y: Double) = math.abs(x - y) < tolerance

def average(x: Double, y: Double) = (x + y) / 2

def fixedPoint(f: (Double) => Double, firstGuess: Double) = {
  def fp(guess: Double, count: Int): Double = {
    val next = f(guess)
    if (closeEnough(guess, next))
      next
    else
      fp(next, count + 1)
  }
  fp(firstGuess, 1)
}
  
def averageDamp(f: (Double) => Double) = (x:Double) => average(x, f(x))

def averageNthOrderDamp(n: Int, f: (Double) => Double) = {
  def anod(i: Int): (Double) => Double = i match {
    case 0 => f
    case _ => averageDamp(anod(i - 1))
  }
  anod(n)
}

def nthPower(x: Double, n: Int): Double = n match {
  case 0 => 1
  case _ => x * nthPower(x, n-1)
}

import org.scalatest._
import org.scalatest.matchers._

object nthPowerSpec extends Spec with ShouldMatchers {
  describe ("nth-power calculator") {
    it ("should calculate the correct values") {
      nthPower(2, 0) should equal (1)
      nthPower(2, 1) should equal (2)
      nthPower(2, 2) should equal (4)
      nthPower(2, 3) should equal (8)
      nthPower(2, 4) should equal (16)
      nthPower(2, 5) should equal (32)
      nthPower(3, 1) should equal (3)
      nthPower(3, 2) should equal (9)
      nthPower(3, 3) should equal (27)
    }
  }
}
nthPowerSpec execute

def averagedXOverYN1(dampCount: Int, n:Int) = (x:Double) =>
    averageNthOrderDamp(dampCount, (y:Double) => x / nthPower(y, n-1))

def nthRoot(x: Double, n: Int, dampCount: Int) = 
  fixedPoint(averagedXOverYN1(dampCount, n)(x), 1.1)

def tryIt(root: Double, n: Int, dampCount: Int) = 
  format("damp count %d, nth root = %f, root^n = %f\n", dampCount, root, nthPower(root,n))

println("square root 2.0 = 1.414213562373095")
tryIt(nthRoot(2.0, 2, 1), 2, 1)
tryIt(nthRoot(2.0, 2, 2), 2, 2)
tryIt(nthRoot(2.0, 2, 3), 2, 3)
tryIt(nthRoot(2.0, 2, 4), 2, 4)

println("Cube root for 2.0 = 1.259921049894873")
tryIt(nthRoot(2.0, 3, 2), 3, 2)
tryIt(nthRoot(2.0, 3, 3), 3, 3)
tryIt(nthRoot(2.0, 3, 4), 3, 4)

println("4th root for 2.0 = 1.189207115002721")
tryIt(nthRoot(2.0, 4, 3), 4, 3)
tryIt(nthRoot(2.0, 4, 4), 4, 4)
tryIt(nthRoot(2.0, 4, 5), 4, 5)

println("5th root for 2.0 = 1.148698354997035")
tryIt(nthRoot(2.0, 5, 4), 5, 4)
tryIt(nthRoot(2.0, 5, 5), 5, 5)
tryIt(nthRoot(2.0, 5, 6), 5, 6)

println("6th root for 2.0 = 1.122462048309373")
tryIt(nthRoot(2.0, 6, 5), 6, 5)
tryIt(nthRoot(2.0, 6, 6), 6, 6)
tryIt(nthRoot(2.0, 6, 7), 6, 7)
