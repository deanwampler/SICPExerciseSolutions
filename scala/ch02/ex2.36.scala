def accumulate[A, B] (op: (A, B) => B, 
                      initial: B, sequence: List[A]): B =
  sequence match {
    case Nil => initial
    case _ => op (sequence.head, accumulate(op, initial, sequence.tail))
  }

def accumulateN[A, B] (op: (A, B) => B, 
                       initial: B, sequences: List[List[A]]): List[B] =
  sequences.head match {
    case Nil => Nil
    case _ => accumulate  (op, initial, 
                accumulate ((x: List[A], l: List[A]) => x.head :: l, Nil, sequences)) ::
              accumulateN (op, initial, 
                accumulate ((x: List[A], l: List[List[A]]) => x.tail :: l, Nil, sequences))
  }

import org.scalatest._ 
import org.scalatest.matchers._

object accumulateNSpec extends Spec with ShouldMatchers {
  describe ("accumulateN") {
    it ("should return a new sequence with the function applied to the corresponding elements of the input sequence of sequences") {
      val seqs = List (List (1, 2, 3), List (4, 5, 6), List (7, 8, 9), List (10, 11, 12))
      accumulateN ((x:Int, y:Int) => x + y, 0, seqs) should equal (List (22, 26, 30))
      accumulateN ((x:Int, y:Int) => x + y, 0, List (List (1), List (2), List (3))) should equal (List (6))
    }
  }
}
accumulateNSpec execute
