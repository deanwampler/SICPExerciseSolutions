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

import org.scalatest._ 
import org.scalatest.matchers._

object foldLeftRightSpec extends Spec with ShouldMatchers {
  describe ("foldLeft") {
    it ("should return a result of a left fold") {
      foldLeft((x:Double, y:Double) => y/x, 1.0, List(1.0, 2.0, 3.0)) should equal (1.0/6.0)
      foldLeft((x:Int, l:List[_]) => List(l,x), Nil, List(1, 2, 3)) should equal (List(List(List(Nil, 1), 2), 3))
    }
  }
  describe ("foldRight") {
    it ("should return a result of a right fold") {
      foldRight((x:Double, y:Double) => x/y, 1.0, List(1.0, 2.0, 3.0)) should equal (3.0/2.0)
      foldRight((x:Int, l:List[_]) => List(x,l), Nil, List(1, 2, 3)) should equal (List(1, List(2, List(3, Nil))))
    }
  }
}
foldLeftRightSpec execute
