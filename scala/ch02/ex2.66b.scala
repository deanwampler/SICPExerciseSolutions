// Compare with ex2.65.scala, which uses Option[Tree], instead of using
// case classes (which is a better approach...)

// Use a pair for the key-value.
type Key = Int
type Value = Int
case class Entry(key: Key, value: Value)

sealed abstract class Tree
case object EmptyTree extends Tree
case class TreeWithBranches(
  entry: Entry, leftBranch: Tree, rightBranch: Tree) extends Tree
// Leaf is not a case class because of equals & hashcode don't work properly
// under inheritance of one case class from another.
class Leaf(entry: Entry) extends TreeWithBranches(entry, EmptyTree, EmptyTree)

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
  
def lookup (key: Key, set: Tree): Option[Value] = set match {
  case EmptyTree => None
  case TreeWithBranches(entry, left, right) => 
    if      (key == entry.key) Some(entry.value)
    else if (key <  entry.key) 
      lookup (key, left)
    else
      lookup (key, right)
}

// Here is the tree for (1 2 3 4 5 6 7 8 9 10), showing the keys only. We'll
// use the 10 * key as the value.
//           5
//       +---+---+
//       3       7
//     +-+-+   +-+-+
//     2   4   6   9
//   +-+-+       +-+-+ 
//   1           8   10

import org.scalatest._ 
import org.scalatest.matchers._

val entries = (0 to 10) map (n => Entry(n, 10 * n)) toArray

val LEAF1  = TreeWithBranches(entries(1),  EmptyTree, EmptyTree)
val LEAF4  = TreeWithBranches(entries(4),  EmptyTree, EmptyTree)
val LEAF6  = TreeWithBranches(entries(6),  EmptyTree, EmptyTree)
val LEAF8  = TreeWithBranches(entries(8),  EmptyTree, EmptyTree)
val LEAF10 = TreeWithBranches(entries(10), EmptyTree, EmptyTree)

val tree = 
  TreeWithBranches(entries(5), 
    TreeWithBranches(entries(3), 
      TreeWithBranches(entries(2), 
        LEAF1, 
        EmptyTree),
      LEAF4),
    TreeWithBranches(entries(7), 
      LEAF6, 
      TreeWithBranches(entries(9), 
        LEAF8,
        LEAF10)))
      

def make10xListTotree = 
  listToTree(entries.slice(1, entries.size).force.toList)._1
  
object lookupSetSpec extends Spec with ShouldMatchers {

  describe ("lookup for trees") {
    it ("should return the value for key if the key-value is in the ordered tree") {
      // Hard-code the tree...
      lookup(7,  tree) should equal (Some(70))
      lookup(11, tree) should equal (None)
      // ... then generate the tree from a list and try that (won't be the same tree...)
      lookup(7,  make10xListTotree) should equal (Some(70))
      lookup(11, make10xListTotree) should equal (None)
    }
  }
}       
lookupSetSpec execute
