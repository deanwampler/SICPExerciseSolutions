// From ex 1.21
def square (n:Int) = n * n

def findDivisor(n: Int, testDivisor: Int): Int = {
  if (square(testDivisor) > n) n
  else if (divides(testDivisor, n)) testDivisor
  else findDivisor(n, testDivisor + 1)
}

def smallestDivisor(n: Int) = findDivisor(n, 2)

def divides(testDivisor: Int, n: Int) = n % testDivisor == 0

def prime(n: Int) = smallestDivisor(n) == n


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

import org.scalatest._ 
import org.scalatest.matchers._

object enumerateIntervalSpec extends Spec with ShouldMatchers {
  describe ("enumerateInterval") {
    it ("should return a list of the integers between start and end, inclusive") {
      enumerateInterval (0, 0)  should equal (List(0))
      enumerateInterval (0, 5)  should equal (List(0, 1, 2, 3, 4, 5))
      enumerateInterval (5, 10) should equal (List(5, 6, 7, 8, 9, 10))
    }
  }
}
enumerateIntervalSpec execute

object uniquePairsSpec extends Spec with ShouldMatchers {
  describe ("uniquePairs") {
    it ("should return a list of pairs (i,j), i<j and 1 <= i <= n") {
      uniquePairs (6) should equal (List(
        (2, 1), (3, 1), (3, 2), (4, 1), (4, 2), (4, 3), 
        (5, 1), (5, 2), (5, 3), (5, 4), 
        (6, 1), (6, 2), (6, 3), (6, 4), (6, 5))) 
    }
  }
}
uniquePairsSpec execute

def pairSum (pair: Pair[Int,Int]) = pair._1 + pair._2

def primeSum (pair: Pair[Int,Int]) = prime (pairSum(pair))

def makePairSum (pair: Pair[Int,Int]) = (pair._1, pair._2, pairSum(pair))

def primeSumPairs (n: Int) = {
  val l = uniquePairs(n) filter primeSum 
  l map makePairSum
}
object primeSumPairsSpec extends Spec with ShouldMatchers {
  describe ("primeSumPairs") {
    it ("should return a list of triplets (i,j, i+j), i<j, 1 <= i <= n, and i+j is prime") {
      primeSumPairs (6) should equal (List( 
              (2, 1, 3), 
              (3, 2, 5),
              (4, 1, 5), (4, 3, 7),
              (5, 2, 7),
              (6, 1, 7), (6, 5, 11)))
    }
  }
}
primeSumPairsSpec execute
