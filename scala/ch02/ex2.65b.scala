// Compare with ex2.65.scala, which uses Option[Tree], instead of using
// case classes (which is a better approach...)

type Entry = Int
sealed abstract class Tree
case object EmptyTree extends Tree
case class TreeWithBranches(
  entry: Entry, leftBranch: Tree, rightBranch: Tree) extends Tree
// Leaf is not a case class because of equals & hashcode don't work properly
// under inheritance of one case class from another.
class Leaf(entry: Entry) extends TreeWithBranches(entry, EmptyTree, EmptyTree)

def elementOfSet (entry: Entry, set: Tree): Boolean = set match {
  case EmptyTree => false
  case TreeWithBranches(e, left, right) => 
    if      (e == entry) true
    else if (e >  entry) 
      elementOfSet (entry, left)
    else
      elementOfSet (entry, right)
}
        
def adjoinSet (entry: Entry, set: Tree): Tree = set match {
  case EmptyTree => new this.Leaf(entry)
  case TreeWithBranches(e, left, right) =>
    if      (e == entry) set
    else if (e >  entry)
      TreeWithBranches (e, adjoinSet (entry, left), right)
    else
      TreeWithBranches (e, left, adjoinSet (entry, right))
}

// Use treeToList2 in Ex. 2.63b.
def treeToList (tree: Tree): List[Entry] = {
  def copyToList (tree: Tree, result: List[Entry]): List[Entry] = tree match {
    case EmptyTree => result
    case TreeWithBranches(e, left, right) =>
      copyToList(left, e :: copyToList(right, result))
  }
  copyToList (tree, Nil)
}

def listToTree (elements: List[Entry]) =
  partialTree (elements, elements.length)
  
def partialTree (elts: List[Entry], n: Int): (Tree, List[Entry]) =
  if (n == 0)
    (EmptyTree, elts)
  else {
    val leftSize      = (n - 1) / 2
    val leftResult    = partialTree (elts, leftSize)
    val leftTree      = leftResult._1
    val nonLeftElts   = leftResult._2
    val rightSize     = n - (leftSize + 1)
    val thisEntry     = nonLeftElts.head
    val rightResult   = partialTree (nonLeftElts.tail, rightSize)
    val rightTree     = rightResult._1
    val remainingElts = rightResult._2
    (TreeWithBranches(thisEntry, leftTree, rightTree), remainingElts)
  }
  
// One way to implement the functions using what we already have is to convert the
// trees to list, compute the union/intersection on the lists, and then convert
// back to trees. It's still O(n), although we'll make at least 3 passes through
// the data.

// Adapted from 2.61:
def elementOfSetList (i:Int, set: List[Int]): Boolean = set match {
  case Nil => false
  case head::tail => 
    if      (head == i) true
    else if (head >  i) false
    else elementOfSetList (i, tail)
}

def intersectionSetLists (set1: List[Entry], set2: List[Entry]): List[Entry] = {
  if      (set1 == Nil || set2 == Nil) Nil
  else if (elementOfSetList (set1.head, set2))
    set1.head :: intersectionSetLists (set1.tail, set2)
  else 
    intersectionSetLists (set1.tail, set2)
}

// Adapted from 2.62:
def unionSetLists (set1: List[Entry], set2: List[Entry]): List[Entry] = {
  if      (set1 == Nil) set2
  else if (set2 == Nil) set1
  else { 
    val i1 = set1.head
    val i2 = set2.head
    if      (i1 == i2)
      i1 :: unionSetLists (set1.tail, set2.tail)
    else if (i1 < i2)
      i1 :: unionSetLists (set1.tail, set2)
    else if (i1 < i2)
      i1 :: unionSetLists (set1.tail, set2)
    else
      i2 :: unionSetLists (set1, set2.tail)
  }
}

def unionSet (set1: Tree, set2: Tree): Tree =
  listToTree (unionSetLists (treeToList (set1), (treeToList (set2))))._1
def intersectionSet (set1: Tree, set2: Tree): Tree =
  listToTree (intersectionSetLists (treeToList (set1), (treeToList (set2))))._1
  
//
// Here is the tree for (1 3 5 7 9 11) - from fig. 2.16:
//           5
//       +---+---+
//       3       9
//     +-+-+   +-+-+
//     1       7   11
//
// Here is the tree for (4 5 6 7 8):
//           6
//       +---+---+
//       5       8
//     +-+-+   +-+-+
//     4       7
//
// The resulting trees should have the following elements:
// union: (1 3 4 5 6 7 8 9 11)
//           6
//       +---+---+
//       3       8
//     +-+-+   +-+-+
//     1   4   7   9
//       +-+-+   +-+-+ 
//           5       11
//
// Note that this is equivalent to:
//           6
//       +---+---+
//       4       9
//     +-+-+   +-+-+
//     3   5   8   11
//   +-+-+   +-+-+ 
//   1       7
// However, the algorithm places unmatched items on the right, rather than the left.
// 
// intersection: (5 7)
//         5
//       +-+-+
//           7
// Similarly, the following tree would also be valid, but doesn't result:
//         7
//       +-+-+
//       5

import org.scalatest._ 
import org.scalatest.matchers._

val LEAF1  = TreeWithBranches(1,  EmptyTree, EmptyTree)
val LEAF4  = TreeWithBranches(4,  EmptyTree, EmptyTree)
val LEAF5  = TreeWithBranches(5,  EmptyTree, EmptyTree)
val LEAF7  = TreeWithBranches(7,  EmptyTree, EmptyTree)
val LEAF8  = TreeWithBranches(8,  EmptyTree, EmptyTree)
val LEAF11 = TreeWithBranches(11, EmptyTree, EmptyTree)

object unionSetSpec extends Spec with ShouldMatchers {

  describe ("unionSet for trees") {
    it ("should return an ordered tree that is the union of two ordered trees") {
      unionSet(
        TreeWithBranches(5, 
          TreeWithBranches(3, LEAF1, EmptyTree),
          TreeWithBranches(9, LEAF7, LEAF11)),
        TreeWithBranches(6, 
          TreeWithBranches(5, LEAF4, EmptyTree),
          TreeWithBranches(7, EmptyTree, LEAF8))) should equal (
        TreeWithBranches(6, 
          TreeWithBranches(3, 
            LEAF1, 
            TreeWithBranches(4, EmptyTree, LEAF5)),
           TreeWithBranches(8, 
            LEAF7, 
            TreeWithBranches(9, EmptyTree, LEAF11))))
    }
  }
}       
unionSetSpec execute
                 
object intersectionSetSpec extends Spec with ShouldMatchers {
  describe ("intersectionSet for trees") {
    it ("should return an ordered tree that is the intersection of two ordered trees") {
      intersectionSet(
        TreeWithBranches(5, 
          TreeWithBranches(3, LEAF1, EmptyTree),
          TreeWithBranches(9, LEAF7, LEAF11)),
        TreeWithBranches(6, 
          TreeWithBranches(5, LEAF4, EmptyTree),
          TreeWithBranches(7, EmptyTree, LEAF8))) should equal (
        TreeWithBranches(5, EmptyTree, LEAF7))
    }
  }
}                        
intersectionSetSpec execute