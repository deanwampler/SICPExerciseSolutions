def accumulate[A, B] (op: (A, B) => B, 
                      initial: B, sequence: List[A]): B =
  sequence match {
    case Nil => initial
    case _ => op (sequence.head, accumulate(op, initial, sequence.tail))
  }

def hornerEval (x: Double, coefficientSequence: List[Double]) = 
  accumulate ((thisCoeff: Double, higherTerms: Double) => thisCoeff + (x * higherTerms), 
              0.0, coefficientSequence)

import org.scalatest._ 
import org.scalatest.matchers._

object hornerEvalSpec extends Spec with ShouldMatchers {
  describe ("hornerEval defined using accumulate") {
    it ("should evaluate a polynomial formula for the specified value of x") {
      hornerEval (2, List(1, 3, 0, 5, 0, 1)) should equal (79)
      hornerEval (2, List(2)) should equal (2)
    }
  }
}
hornerEvalSpec execute

