// For testing purposes, we'll just use a List of an arbitrary element for "painter":
// To distinguish beside from below, use a List of two elements for beside and a
// List of 2 List for below.
type Painter = List[Any]
def beside(p1: Painter, p2: Painter) = List(p1, p2)
def below(p1: Painter, p2: Painter) = List(List(p1), List(p2))
  
def rightSplitOld(painter: Painter, n: Int): Painter = 
  if (n == 0)
    painter
  else {
    val smaller = rightSplit(painter, n - 1)
    beside(painter, below(smaller, smaller))
  }
        
def upSplitOld(painter: Painter, n: Int): Painter = 
  if (n == 0)
    painter
  else {
    val smaller = upSplit(painter, n - 1)
    below(painter, beside(smaller, smaller))
  }
        
def split(f1: (Painter, Painter) => Painter, 
          f2: (Painter, Painter) => Painter): (Painter, Int) => Painter = {
  def splt(painter: Painter, n: Int): Painter =
    if (n == 0)
      painter
    else {
      val smaller = splt(painter, n - 1)
      f1(painter, f2(smaller, smaller))
    }
  
  (painter: Painter, n: Int) => splt(painter, n)  // return a lambda wrapping splt
}
  
def rightSplit(painter: Painter, n: Int): Painter = 
  split(beside, below)(painter, n)
def upSplit(painter: Painter, n: Int): Painter = 
  split(below, beside)(painter, n)

import org.scalatest._ 
import org.scalatest.matchers._

object splitSpec extends Spec with ShouldMatchers {
  describe ("rightSplit") {
    it ("should return the same result as the original rightSplit") {
      rightSplit(List(1), 1) should equal (rightSplitOld(List(1), 1))
      rightSplit(List(1), 2) should equal (rightSplitOld(List(1), 2))
    }
  }
  describe ("upSplit") {
    it ("should return the same result as the original upSplit") {
      upSplit(List(1), 1) should equal (upSplitOld(List(1), 1))
      upSplit(List(1), 2) should equal (upSplitOld(List(1), 2))
    }
  }
}
splitSpec execute