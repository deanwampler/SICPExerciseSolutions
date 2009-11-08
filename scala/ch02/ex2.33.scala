def accumulate[A, B] (op: (A, B) => B, 
                      initial: B, sequence: List[A]): B =
  sequence match {
    case Nil => initial
    case _ => op (sequence.head, accumulate(op, initial, sequence.tail))
  }

def map[A, B] (p: (A) => B, sequence: List[A]) = 
  accumulate ((a:A, l:List[B]) => p(a) :: l, Nil, sequence)
  
import org.scalatest._ 
import org.scalatest.matchers._

object mapSpec extends Spec with ShouldMatchers {
  describe ("map defined using accumulate") {
    it ("should return a new collection with the elements mapped") {
      map ((i:Int) => i*i, Nil) should equal (Nil)
      map ((i:Int) => i*i, List(2)) should equal (List(4))
      map ((i:Int) => i*i, List(1, 2, 3, 4, 5)) should equal (List(1, 4, 9, 16, 25))
    }
  }
}
mapSpec execute

def append[A] (seq1: List[A], seq2: List[A]) = 
  accumulate ((a:A, l:List[A]) => a :: l, seq2, seq1)
  
object appendSpec extends Spec with ShouldMatchers {
  describe ("append defined using accumulate") {
    it ("should return a new collection that joins the input sequences") {
      append (Nil, Nil) should equal (Nil)
      append (List(1), Nil) should equal (List(1))
      append (Nil, List(2)) should equal (List(2))
      append (List(1, 2, 3, 4), List(5, 6, 7, 8)) should equal (List(1,2,3,4,5,6,7,8))
    }
  }
}
appendSpec execute

// "length" would collide with another method in scope with that name.
def length2[A] (sequence: List[A]) =
  accumulate ((a:Any, count:Int) => count + 1, 0, sequence)
  
object lengthSpec extends Spec with ShouldMatchers {
  describe ("length2 defined using accumulate") {
    it ("should return a the length of the input collection") {
      length2 (Nil) should equal (0)
      length2 (List(1)) should equal (1)
      length2 (List(1, 2, 3, 4, 5, 6, 7, 8)) should equal (8)
    }
  }
}
lengthSpec execute
