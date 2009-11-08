def foldRight[A, B] (op: (A, B) => B, initial: B, sequence: List[A]): B =
  sequence match {
    case Nil => initial
    case _ => op (sequence.head, foldRight(op, initial, sequence.tail))
  }

def foldLeft[A, B] (op: (A, B) => B, initial: B, sequence: List[A]): B = {
  def iter(result: B, rest:List[A]): B =
    rest match {
      case Nil => result
      case _ => iter(op (rest.head, result), rest.tail)
    }
  iter(initial, sequence)
}

// Expect a warning for the + method:
def reverseRight[A] (sequence: List[A]) =
  foldRight ((a:A, l:List[A]) => l + a, Nil, sequence)

def reverseLeft[A] (sequence: List[A]) =
  foldLeft ((a: A, l:List[A]) => a :: l, Nil, sequence)
  
import org.scalatest._ 
import org.scalatest.matchers._

object reverseLeftRightSpec extends Spec with ShouldMatchers {
  describe ("reverseLeft") {
    it ("should return a reversed list") {
      reverseLeft (List(1, 2, 3)) should equal (List(3, 2, 1))
      reverseLeft (List(1)) should equal (List(1))
      reverseLeft (Nil) should equal (Nil)
    }
  }
  describe ("reverseRight") {
    it ("should return a reversed list") {
      reverseRight (List(1, 2, 3)) should equal (List(3, 2, 1))
      reverseRight (List(1)) should equal (List(1))
      reverseRight (Nil) should equal (Nil)
    }
  }
}
reverseLeftRightSpec execute
