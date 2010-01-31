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

def adjoinSet (tree: Tree, set: List[Tree]): List[Tree] = set match {
  case Nil => List(tree)
  case head :: tail => 
    if (tree.weight < head.weight)
      tree :: set
    else
      head :: adjoinSet(tree, tail)
}
                    
// An alternative would be to take a list of Leaves, in which case makeLeafSet
// would be just a sorting routine.
def makeLeafSet (pairs: List[Pair[Symbol, Weight]]): List[Tree] = pairs match {
  case Nil => Nil
  case head :: tail => adjoinSet(Leaf(head._1, head._2), makeLeafSet(tail))
}
                    
def generateHuffmanTree (pairs: List[Pair[Symbol, Weight]]) = 
  successiveMerge (makeLeafSet (pairs))

// Iterate through the set, using adjoinSet to insert at the proper location
// the new subtree that replaces the two lowest-ranked elements in the set (which
// could be either pairs or subtrees).
def successiveMerge (leafSet: List[Tree]): Tree = leafSet match {
  case head :: Nil => head   // when 1, we have 1 tree, rather than a set of leaves
  case head :: tail => successiveMerge (adjoinSet (makeCodeTree (head, tail.first), tail.tail))
  case Nil => throw new RuntimeException("Invalid leaf set: "+leafSet)
}
    
val huffmanTree = 
  generateHuffmanTree(
    List(('a, 2), ('boom, 1), ('get, 2), ('job, 2), ('na, 16), ('sha, 3), ('yip, 9), ('wah, 1)))

val messageEncoding =
  encode( 
    List('get, 'a, 'job, 
      'sha, 'na, 'na, 'na, 'na, 'na, 'na, 'na, 'na, 
      'get, 'a, 'job, 
      'sha, 'na, 'na, 'na, 'na, 'na, 'na, 'na, 'na, 
      'wah, 'yip, 'yip, 'yip, 'yip, 'yip, 'yip, 'yip, 'yip, 'yip, 
      'sha, 'boom),
    huffmanTree) 

import org.scalatest._ 
import org.scalatest.matchers._

object variableLengthSymbolHuffmanEncodingSpec extends Spec with ShouldMatchers {
  describe ("encode with variable length symbols") {
    it ("should handle variable length symbols") {
      encode (List('a),    huffmanTree) should equal (List(One, One, Zero, Zero))
      encode (List('boom), huffmanTree) should equal (List(One, One, Zero, One, One))
      encode (List('get),  huffmanTree) should equal (List(One, One, One, One, One))
      encode (List('job),  huffmanTree) should equal (List(One, One, One, One, Zero))
      encode (List('na),   huffmanTree) should equal (List(Zero))
      encode (List('sha),  huffmanTree) should equal (List(One, One, One, Zero))
      encode (List('yip),  huffmanTree) should equal (List(One, Zero))
      encode (List('wah),  huffmanTree) should equal (List(One, One, Zero, One, Zero))
      messageEncoding.size should equal (84)
    }
  }
}
variableLengthSymbolHuffmanEncodingSpec execute

// So, 84 bits are required to encode this message. 
// With a fixed-length representation, we would need 3 bits (log_2(8)) times the 
// number of symbols, 37, so we would need 3 * 37 = 111.
