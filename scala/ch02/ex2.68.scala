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
  
def encodeSymbol (symbol: Symbol, tree: Tree) = {
  def encodeSym (subtree: Tree, result: List[Bit]): List[Bit] = subtree match {
    case Leaf(sym, _) => if (sym == symbol) result else Nil
    case twb:TreeWithBranches => 
      val leftResult  = encodeSym (twb.leftBranch,  Zero :: result)
      val rightResult = encodeSym (twb.rightBranch, One  :: result)
      if      (leftResult  != Nil) leftResult
      else if (rightResult != Nil) rightResult
      else Nil
  }
  encodeSym(tree, Nil).reverse match {
    case Nil => throw new DecodeException("Could not encode symbol")
    case answer => answer
  }
}

def encode (message: List[Symbol], tree: Tree): List[Bit] = message match {
  case Nil => Nil
  case head :: tail => encodeSymbol (head, tree) ++ encode (tail, tree)
}

val sampleTree = 
  makeCodeTree(
    Leaf ('A, 4),
    makeCodeTree(
      Leaf ('B, 2),
      makeCodeTree(
        Leaf ('D, 1),
        Leaf ('C, 1))))
        
val expectedMessage =
  List(Zero, One, One, Zero, Zero, One, Zero, 
       One, Zero, One, One, One, Zero)

import org.scalatest._ 
import org.scalatest.matchers._

object huffmanEncodeSpec extends Spec with ShouldMatchers {

  describe ("encode") {
    it ("should return the sequence of bits for a sequence of letters") {
      encode (List('A), sampleTree) should equal (List(Zero))
      encode (List('B), sampleTree) should equal (List(One, Zero))
      encode (List('C), sampleTree) should equal (List(One, One, One))
      encode (List('D), sampleTree) should equal (List(One, One, Zero))
      evaluating { 
        encode (List('E), sampleTree) 
      } should produce [DecodeException]
      encode (List('A, 'D, 'A, 'B, 'B, 'C, 'A), sampleTree) should equal (expectedMessage)
    }
  }
}
huffmanEncodeSpec execute
