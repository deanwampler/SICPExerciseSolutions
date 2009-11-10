def foldRight[A, B] (op: (A, B) => B, initial: B, sequence: List[A]): B =
  sequence match {
    case Nil => initial
    case _ => op (sequence.head, foldRight(op, initial, sequence.tail))
  }

// We use List's builtin map operation.
def flatmap[A,B] (proc: (A) => List[B], seq:List[A]) =
  foldRight((l1:List[B], l2:List[B]) => l1 ++ l2, Nil, seq map proc)

def enumerateInterval (start: Int, end: Int) = {
  def iter (result:List[Int], n:Int): List[Int] = 
    if (n > end)
      result
    else
      iter (result ++ List(n), n + 1)
  iter(Nil, start)
}

def uniquePairs (n: Int) = 
  flatmap ((i:Int) => enumerateInterval(1, i-1) map (Pair(i,_)),
    enumerateInterval(1, n))

def uniqueTriplets (n: Int) = 
  flatmap ((k:Int) =>  uniquePairs(k-1).map((pair:Pair[Int,Int]) => (k, pair._1, pair._2)),
           enumerateInterval(1, n))

import org.scalatest._ 
import org.scalatest.matchers._

object uniqueTripletsSpec extends Spec with ShouldMatchers {
  describe ("uniqueTriplets") {
    it ("should return a list of triplets (i,j,k), i>j>k and 1 <= i <= n") {
      uniqueTriplets (6) should equal (List(
        (3, 2, 1), 
        (4, 2, 1), (4, 3, 1), (4, 3, 2), 
        (5, 2, 1), (5, 3, 1), (5, 3, 2), (5, 4, 1), (5, 4, 2), (5, 4, 3),
        (6, 2, 1), (6, 3, 1), (6, 3, 2), (6, 4, 1), (6, 4, 2), (6, 4, 3),
        (6, 5, 1), (6, 5, 2), (6, 5, 3), (6, 5, 4) ))
    }
  }
}
uniqueTripletsSpec execute

// Let's generalize this to arbitrary tuples, but we have to use Lists, since there
// is no uniform way to create a tupleN from a tupleN-1.

def uniqueTuples (arity:Int, n:Int): List[List[Int]] =
  if (arity == 1)
    enumerateInterval (1, n) map (List(_))
  else
    flatmap ((k:Int) => uniqueTuples (arity - 1, k - 1) map ((l:List[Int]) => k :: l),
              enumerateInterval(1, n))

;
object uniqueTuplesSpec extends Spec with ShouldMatchers {
  describe ("uniqueTuples") {
    it ("should return a list of lists of N (i,j, ..., m) unique integers where each integer is <= i <= n") {
      uniqueTuples (0, 6) should equal (Nil)
      uniqueTuples (1, 6) should equal (List(List(1), List(2), List(3), List(4), List(5), List(6)))
      uniqueTuples (2, 6) should equal (List(
        List(2, 1), List(3, 1), List(3, 2), List(4, 1), List(4, 2), List(4, 3), 
        List(5, 1), List(5, 2), List(5, 3), List(5, 4), 
        List(6, 1), List(6, 2), List(6, 3), List(6, 4), List(6, 5))) 
      uniqueTuples (3, 6) should equal (List(
        List(3, 2, 1), 
        List(4, 2, 1), List(4, 3, 1), List(4, 3, 2), 
        List(5, 2, 1), List(5, 3, 1), List(5, 3, 2), List(5, 4, 1), List(5, 4, 2), List(5, 4, 3),
        List(6, 2, 1), List(6, 3, 1), List(6, 3, 2), List(6, 4, 1), List(6, 4, 2), List(6, 4, 3),
        List(6, 5, 1), List(6, 5, 2), List(6, 5, 3), List(6, 5, 4) ))
      uniqueTuples (4, 5) should equal (List(
        List(4, 3, 2, 1),
        List(5, 3, 2, 1), List(5, 4, 2, 1), List(5, 4, 3, 1), List(5, 4, 3, 2) ))
    }
  }
}
uniqueTuplesSpec execute
