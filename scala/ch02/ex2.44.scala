// For testing purposes, we'll just use a List of an arbitrary element for "painter":
// To distinguish beside from below, use a List of two elements for beside and a
// List of 2 List for below.
type Painter = List[Any]
def beside(p1: Painter, p2: Painter) = List(p1, p2)
def below(p1: Painter, p2: Painter) = List(List(p1), List(p2))
  
def rightSplit(painter: Painter, n: Int): Painter = 
  if (n == 0)
    painter
  else {
    val smaller = rightSplit(painter, n - 1)
    beside(painter, below(smaller, smaller))
  }
        
def upSplit(painter: Painter, n: Int): Painter = 
  if (n == 0)
    painter
  else {
    val smaller = upSplit(painter, n - 1)
    below(painter, beside(smaller, smaller))
  }
        
def cornerSplit(painter: Painter, n: Int): Painter = 
  if (n == 0)
    painter
  else {
    val up = upSplit(painter, n - 1)
    val right = rightSplit(painter, n - 1)
    val topLeft = beside(up, up)
    val bottomRight = below(right, right)
    val corner = cornerSplit(painter, n - 1)
    beside(below(painter, topLeft),
           below(bottomRight, corner))
  }
           
import org.scalatest._ 
import org.scalatest.matchers._

object splitSpec extends Spec with ShouldMatchers {
  describe ("rightSplit") {
    it ("should return a 2-element list with painter and a list of 2, single-element lists of painter") {
      rightSplit(List(1), 1)  should equal (List(List(1), List(List(List(1)), List(List(1)))))
    }
  }
  describe ("upSplit") {
    it ("should return a 2-element list with a list of a list of 2 painter elements, followed by a list of painter") {
      upSplit(List(1), 1)  should equal (List(List(List(1)), List(List(List(1), List(1)))))
    }
  }
  describe ("cornerSplit") {
    it ("should return a complicated list of rightSplit and upSplit elements") {
      cornerSplit(List(1), 1)  should equal (List(List(List(List(1)), List(List(List(1), List(1)))), List(List(List(List(List(1)), List(List(1)))), List(List(1)))))
    }
  }
}
splitSpec execute