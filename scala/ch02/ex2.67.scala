type Weight = Int

// In contrast with the 2.6Nb exercises, we'll make Leaf a case class and not
// a subclass of TreeWithBranches. Also, we eliminated EmptyTree.
sealed abstract class Tree(val symbols: List[Symbol], val weight: Weight)
case class TreeWithBranches(
  leftBranch: Tree, rightBranch: Tree, 
  override val symbols: List[Symbol], 
  override val weight: Weight) 
    extends Tree(symbols, weight)
case class Leaf(symbol: Symbol, override val weight: Weight) 
    extends Tree(List(symbol), weight)

def makeCodeTree (left: Tree, right: Tree) = 
  TreeWithBranches(
    left, 
    right, 
    left.symbols ++ right.symbols, 
    left.weight + right.weight)
              
sealed abstract class Bit(val value: Int)
case object Zero extends Bit(0)
case object One  extends Bit(1)

class DecodeException(message: String) extends RuntimeException(message)

def decode (bits: List[Bit], tree: TreeWithBranches): List[Symbol] = {
  def decode1 (bits: List[Bit], current: TreeWithBranches): List[Symbol] = bits match {
    case Nil => Nil
    case head :: tail => chooseBranch (head, current) match {
      case Leaf(symbol, _) => symbol :: decode1(tail, tree)
      case next:TreeWithBranches => decode1(tail, next)
    }
  }
  decode1 (bits, tree)
}
  
def chooseBranch (bit: Bit, branch: TreeWithBranches) = bit match {
  case Zero => branch.leftBranch
  case One  => branch.rightBranch
}
  
val sampleTree = 
  makeCodeTree(
    Leaf ('A, 4),
    makeCodeTree(
      Leaf ('B, 2),
      makeCodeTree(
        Leaf ('D, 1),
        Leaf ('C, 1))))
        
val sampleMessage =
  List(Zero, One, One, Zero, Zero, One, Zero, 
       One, Zero, One, One, One, Zero)

import org.scalatest._ 
import org.scalatest.matchers._

object huffmanDecodeSpec extends Spec with ShouldMatchers {

  describe ("decode") {
    it ("should return the sequence of letters from a bit encoding") {
      decode (sampleMessage, sampleTree) should equal (
        List('A, 'D, 'A, 'B, 'B, 'C, 'A))
    }
  }
}
huffmanDecodeSpec execute
