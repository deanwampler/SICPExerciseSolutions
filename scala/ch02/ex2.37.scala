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

def mapLists[A](op: (A,A) => A, l1: List[A], l2: List[A]) = 
  l1 zip(l2) map { (pair:(A,A)) => op(pair._1, pair._2) }
  
def dotProduct (v1: List[Int], v2: List[Int]) = 
 accumulate ((i:Int,j:Int) => i + j, 0, mapLists ((i:Int,j:Int) => i * j, v1, v2))

def matrixTimesVector (m: List[List[Int]], v: List[Int]) = 
  m map ((mi: List[Int]) => accumulate((i:Int,j:Int) => i + j, 0, mapLists((i:Int,j:Int) => i * j, mi, v)))

def transpose (m: List[List[Int]]) = 
  accumulateN((i:Int, l:List[Int]) => i :: l, Nil, m)

def matrixTimesMatrix (m1: List[List[Int]], m2: List[List[Int]]) = {
  val cols = transpose (m2)
  m1 map ((row:List[Int]) => cols map ((col:List[Int]) => dotProduct (row, col)))
}

import org.scalatest._ 
import org.scalatest.matchers._

object dotProductSpec extends Spec with ShouldMatchers {
  describe ("dotProduct") {
    it ("should return an integer that is the dot product of two vectors") {
      // () . () = 0
      // (2) . (4) = 8
      // (1 2 3) * (4 5 6) = 32
      dotProduct (Nil, Nil) should equal (0)
      dotProduct (List(2), List(4)) should equal (8)
      dotProduct (List(1, 2, 3), List(4, 5, 6)) should equal (32)
    }
  }
}
dotProductSpec execute

object matrixTimesVectorSpec extends Spec with ShouldMatchers {
  describe ("matrixTimesVector") {
    it ("should return a new vector that is the matrix times the input vector") {
      // (() () ()) * () = (0 0 0)
      // ((1) (2) (3)) * (2) = (2 4 6)
      // ((1 2 3) (4 5 6) (7 8 9)) * (2 4 6) = (28 64 100)
      matrixTimesVector (List(Nil, Nil, Nil), Nil) should equal (List(0, 0, 0))
      matrixTimesVector (List(List(1), List(2), List(3)), List(2)) should equal (List(2, 4, 6))
      matrixTimesVector (List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)), List(2, 4, 6)) should equal (List(28, 64, 100))
    }
  }
}
matrixTimesVectorSpec execute


object transposeSpec extends Spec with ShouldMatchers {
  describe ("transpose") {
    it ("should return the transpose of a matrix") {
      // tr((1) (2) (3)) = ((1 2 3))
      // tr((1 2 3) (4 5 6) (7 8 9)) = ((1 4 7) (2 5 8) (3 6 9))
      transpose (List(List(1), List(2), List(3))) should equal (List(List(1, 2, 3)))
      transpose (List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) should equal (List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9)))
    }
  }
}
transposeSpec execute

object matrixTimesMatrixSpec extends Spec with ShouldMatchers {
  describe ("matrixTimesMatrix") {
    it ("should return a new matrix that is the product of the input matrices") {
      // ((1)) * ((2)) = ((2))
      // ((1 2) (3 4)) * ((5 6) (7 8)) = ((19 22) (43 50))
      matrixTimesMatrix (List(List(1)), List(List(2))) should equal (List(List(2)))
      matrixTimesMatrix (List(List(1, 2), List(3, 4)), List(List(5, 6), List(7, 8))) should equal (List(List(19, 22), List(43, 50)))
    }
  }
}
matrixTimesMatrixSpec execute


