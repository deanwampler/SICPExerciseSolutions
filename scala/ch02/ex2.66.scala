// Use a pair for the key-value.
type Key = Int
type Value = Int
case class Entry(key: Key, value: Value)
case class Tree(entry: Entry, leftBranch: Option[Tree], rightBranch: Option[Tree])

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
  
def lookup (key: Key, set: Option[Tree]): Option[Value] = set match {
  case None => None
  case Some(s) => 
    if      (key == s.entry.key) Some(s.entry.value)
    else if (key <  s.entry.key) 
      lookup (key, s.leftBranch)
    else
      lookup (key, s.rightBranch)
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

val LEAF1  = Some(Tree(entries(1),  None, None))
val LEAF4  = Some(Tree(entries(4),  None, None))
val LEAF6  = Some(Tree(entries(6),  None, None))
val LEAF8  = Some(Tree(entries(8),  None, None))
val LEAF10 = Some(Tree(entries(10), None, None))

val tree = 
  Some(Tree(entries(5), 
    Some(Tree(entries(3), 
      Some(Tree(entries(2), 
        LEAF1, 
        None)),
      LEAF4)),
    Some(Tree(entries(7), 
      LEAF6, 
      Some(Tree(entries(9), 
        LEAF8,
        LEAF10))))))
      

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
