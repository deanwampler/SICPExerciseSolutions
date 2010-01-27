// Compare with ex2.64.scala, which uses Option[Tree], instead of using
// case classes (which is a better approach...)

type Entry = Int
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
  
import org.scalatest._ 
import org.scalatest.matchers._

object listToTreeSpec extends Spec with ShouldMatchers {
  describe ("listToTree") {
    it ("should return an ordered tree from an ordered lists") {
      listToTree(List(1, 3, 5, 7, 9, 11)) should equal (
        (TreeWithBranches(5, 
          TreeWithBranches(1, 
            EmptyTree, 
            TreeWithBranches(3, EmptyTree, EmptyTree)),
          TreeWithBranches(9, 
            TreeWithBranches(7, EmptyTree, EmptyTree), 
            TreeWithBranches(11, EmptyTree, EmptyTree))),
         Nil)
        )
    }
  }
}
listToTreeSpec execute
  
// a. For the tree to be balanced, we want the top "root" element to be the number
// at middle of the list. If there is an odd number N of elements, it will be the
// element at position N/2 (counting from 0). If there is an even number of 
// elements, it will still be position N/2, but this element will actually be 
// the last element in the first half of the list. Actually, the algorithm selects
// that element by dividing the list along the (N-1)/2 position. Then the top 
// element will be the car of the second half of the list.
// The algorithm then recursively forms a tree from each half. For each subtree,
// the middle element becomes the root.
// The algorithm uses the "remaining elements" as a pool from which to draw the
// next elements to process.
// Here is the tree for (1 3 5 7 9 11):
//           5
//       +---+---+
//       1       9
//     +-+-+   +-+-+
//         3   7   11
//
// b. It's O(n), because the algorithm traverses every element once.