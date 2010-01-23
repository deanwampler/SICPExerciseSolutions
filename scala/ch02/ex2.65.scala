type Entry = Int
case class Tree(entry: Entry, leftBranch: Option[Tree], rightBranch: Option[Tree])

def elementOfSet (x: Entry, set: Option[Tree]): Boolean = set match {
  case None => false
  case Some(s) => 
    if      (x == s.entry) true
    else if (x <  s.entry) 
      elementOfSet (x, s.leftBranch)
    else
      elementOfSet (x, s.rightBranch)
}

def adjoinSet (x: Entry, set: Option[Tree]): Option[Tree] = set match {
  case None => Some(Tree(x, None, None))
  case Some(s) =>
    if      (x == s.entry) set
    else if (x <  s.entry)
      Some(Tree (s.entry, adjoinSet (x, s.leftBranch), s.rightBranch))
    else
      Some(Tree (s.entry, s.leftBranch, adjoinSet (x, s.rightBranch)))
}

// Use treeToList2 in Ex. 2.63.
def treeToList (tree: Option[Tree]): List[Entry] = {
  def copyToList (tree: Option[Tree], result: List[Entry]): List[Entry] = tree match {
    case None => result
    case Some(t) => copyToList(t.leftBranch, t.entry :: copyToList(t.rightBranch, result))
  }
  copyToList (tree, Nil)
}

def listToTree (elements: List[Entry]) =
  partialTree (elements, elements.length)
  
def partialTree (elts: List[Entry], n: Int): (Option[Tree], List[Entry]) =
  if (n == 0)
    (None, elts)
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
    (Some(Tree(thisEntry, leftTree, rightTree)), remainingElts)
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

def unionSet (set1: Option[Tree], set2: Option[Tree]): Option[Tree] =
  listToTree (unionSetLists (treeToList (set1), (treeToList (set2))))._1
def intersectionSet (set1: Option[Tree], set2: Option[Tree]): Option[Tree] =
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

val LEAF1  = Some(Tree(1,  None, None))
val LEAF4  = Some(Tree(4,  None, None))
val LEAF5  = Some(Tree(5,  None, None))
val LEAF7  = Some(Tree(7,  None, None))
val LEAF8  = Some(Tree(8,  None, None))
val LEAF11 = Some(Tree(11, None, None))

object unionSetSpec extends Spec with ShouldMatchers {

  describe ("unionSet for trees") {
    it ("should return an ordered tree that is the union of two ordered trees") {
      unionSet(
        Some(Tree(5, Some(Tree(3, LEAF1, None)),
                     Some(Tree(9, LEAF7, LEAF11)))),
        Some(Tree(6, Some(Tree(5, LEAF4, None)),
                     Some(Tree(7, None, LEAF8))))) should equal (
      Some(Tree(6, Some(Tree(3, LEAF1, Some(Tree(4, None, LEAF5)))),
                   Some(Tree(8, LEAF7, Some(Tree(9, None, LEAF11)))))))
    }
  }
}       
unionSetSpec execute
                 
object intersectionSetSpec extends Spec with ShouldMatchers {
  describe ("intersectionSet for trees") {
    it ("should return an ordered tree that is the intersection of two ordered trees") {
      intersectionSet(
        Some(Tree(5, Some(Tree(3, LEAF1, None)),
                     Some(Tree(9, LEAF7, LEAF11)))),
        Some(Tree(6, Some(Tree(5, LEAF4, None)),
                     Some(Tree(7, None, LEAF8))))) should equal (
      Some(Tree(5, None, LEAF7)))
    }
  }
}                        
intersectionSetSpec execute