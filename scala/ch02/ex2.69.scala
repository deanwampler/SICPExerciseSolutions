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
                    
import org.scalatest._ 
import org.scalatest.matchers._

object makeLeafSetSpec extends Spec with ShouldMatchers {
  describe ("makeLeafSet") {
    it ("should return an ordered sequence of Leaf objects") {
      makeLeafSet (List(('A, 4), ('B, 2), ('C, 1), ('D, 1))) should equal (
       List(Leaf('D, 1), Leaf('C, 1), Leaf('B, 2), Leaf('A, 4)))
    }
  }
}
makeLeafSetSpec execute

def generateHuffmanTree (pairs: List[Pair[Symbol, Weight]]) = 
  successiveMerge (makeLeafSet (pairs))

// Iterate through the set, using adjoinSet to insert at the proper location
// the new subtree that replaces the two lowest-ranked elements in the set (which
// could be either pairs or subtrees).
def successiveMerge (leafSet: List[Tree]): Tree = leafSet match {
  case head :: Nil => head   // when 1, we have 1 tree, rather than a set of leaves
  case head :: tail => successiveMerge (adjoinSet (makeCodeTree (head, tail.head), tail.tail))
  case Nil => throw new RuntimeException("Invalid leaf set: "+leafSet)
}
    
// Starting with 'List((A, 4), (B, 2), (C, 1), (D, 1)), we should get the following encoding:
//  
//          + ((A B C D) 8)
//     +----+----+
//     + (A 4)   + ((B C D) 4)
//         +-----+-----+
//         + (B 2)     + ((D C) 2) 
//             +-------+-------+
//             + (D 1)         + (C 1)
//
// Resulting tree is shown in the next test:

object generateHuffmanTreeSpec extends Spec with ShouldMatchers {
  describe ("generateHuffmanTree") {
    it ("should return tree with the Huffman encoding") {
      generateHuffmanTree (List(('C, 1), ('D, 1))) should equal (
      TreeWithBranches(
        Leaf('D, 1),
        Leaf('C, 1),
        List('D, 'C),
        2))
              
      generateHuffmanTree (List(('A, 4), ('B, 2), ('C, 1), ('D, 1))) should equal (
      TreeWithBranches(
        Leaf('A, 4),
        TreeWithBranches(
          Leaf('B, 2),
          TreeWithBranches(
            Leaf('D, 1),
            Leaf('C, 1),
            List('D, 'C),
            2),
          List('B, 'D, 'C),
          4),
        List('A, 'B, 'D, 'C),
        8))
              
      generateHuffmanTree (
        List(('A, 8), ('B, 3), ('C, 1), ('D, 1), 
             ('E, 1), ('F, 1), ('G, 1), ('H, 1))) should equal (
      TreeWithBranches(
        Leaf('A, 8),
        TreeWithBranches(
          TreeWithBranches(
            TreeWithBranches(
              Leaf('H, 1),
              Leaf('G, 1),
              List('H, 'G),
              2),
            TreeWithBranches(
              Leaf('F, 1),
              Leaf('E, 1),
              List('F, 'E),
              2),
            List('H, 'G, 'F, 'E),
            4),
            TreeWithBranches(
              TreeWithBranches(
                Leaf('D, 1),
                Leaf('C, 1),
                List('D, 'C),
                2),
              Leaf('B, 3),
              List('D, 'C, 'B),
              5),
            List('H, 'G, 'F, 'E, 'D, 'C, 'B),
            9),
          List('A, 'H, 'G, 'F, 'E, 'D, 'C, 'B),
          17))
    }
  }
}
generateHuffmanTreeSpec execute