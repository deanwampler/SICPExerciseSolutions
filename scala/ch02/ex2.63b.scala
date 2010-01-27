// Compare with ex2.63.scala, which uses Option[Tree], instead of using
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

def treeToList1 (tree: Tree): List[Entry] = tree match {
  case EmptyTree => Nil
  case TreeWithBranches(e, left, right) =>
    treeToList1(left) ++ (e :: treeToList1(right))
}

def treeToList2 (tree: Tree): List[Entry] = {
  def copyToList (tree: Tree, result: List[Entry]): List[Entry] = tree match {
    case EmptyTree => result
    case TreeWithBranches(e, left, right) =>
      copyToList(left, e :: copyToList(right, result))
  }
  copyToList (tree, Nil)
}

val tree1 = 
  TreeWithBranches(7, 
    TreeWithBranches(3, 
      TreeWithBranches(1, EmptyTree, EmptyTree), 
      TreeWithBranches(5, EmptyTree, EmptyTree)), 
    TreeWithBranches(9, 
      EmptyTree, 
      TreeWithBranches(11, EmptyTree, EmptyTree)))
  
val tree2 = 
  TreeWithBranches(3, 
    TreeWithBranches(1, EmptyTree, EmptyTree),
    TreeWithBranches(7, 
         TreeWithBranches(5, EmptyTree, EmptyTree), 
         TreeWithBranches(9, 
          EmptyTree, 
          TreeWithBranches(11, EmptyTree, EmptyTree))))

val tree3 = 
  TreeWithBranches(5,
    TreeWithBranches(3, 
      TreeWithBranches(1, EmptyTree, EmptyTree), 
      EmptyTree),
    TreeWithBranches(9, 
      TreeWithBranches(7, EmptyTree, EmptyTree), 
      TreeWithBranches(11, EmptyTree, EmptyTree)))

import org.scalatest._ 
import org.scalatest.matchers._

object treeToListSpec extends Spec with ShouldMatchers {
  describe ("treeToList1 and treeToList2") {
    it ("should return the same ordered lists from an input ordered tree") {
      treeToList1(tree1) should equal (treeToList2(tree1))
      treeToList1(tree2) should equal (treeToList2(tree2))
      treeToList1(tree3) should equal (treeToList2(tree3))
    }
  }
}
treeToListSpec execute

// a. Both treeToList functions traverse the structure depth-first, appending to
// the resulting vect the (left entry right) at each level, where left and right
// will be built up from the lower layers first. Therefore, both functions produce
// the same sorted vect output as the checks demonstrate.
//
// b. Both functions traverse all elements of the tree, an O(n) process, through
// the common structure of the form
//  (f (leftBranch) (cons (entry) (f (rightBranch) ...)
// where "f" is either treeToList1 or copy-to-vect (for treeToList2). 
// Hence, these parts of the functions have roughly the same order.
// treeToList2 traverses each node once as it cons the element to the result-vect.
// However, treeToList1 traverses the nodes roughly O(n) again when it appends to
// the end of vects. So, it grows ~ 2x as fast.
// Here is a rough test (it has overhead from the test itself).

def runtime = System.currentTimeMillis

def timeTreeToVect (
    n: Int, 
    whichRun: String, 
    treeToListProc: Tree => List[Entry],
    tree: Tree) = {
  format ("starting %s: ", whichRun)
  val startTime = runtime
  for (i <- 1 to n)
    treeToListProc(tree)
  println (runtime - startTime)
}
  
timeTreeToVect (100000, "treeToList1 tree1", treeToList1, tree1)
timeTreeToVect (100000, "treeToList2 tree1", treeToList2, tree1)
timeTreeToVect (100000, "treeToList1 tree2", treeToList1, tree2)
timeTreeToVect (100000, "treeToList2 tree2", treeToList2, tree2)
timeTreeToVect (100000, "treeToList1 tree3", treeToList1, tree3)
timeTreeToVect (100000, "treeToList2 tree3", treeToList2, tree3)
// output (it varies from run to run):
// starting treeToList1 tree1: 92
// starting treeToList2 tree1: 50
// starting treeToList1 tree2: 31
// starting treeToList2 tree2: 17
// starting treeToList1 tree3: 34
// starting treeToList2 tree3: 17

// If you compare these results to the results for the Scheme and Clojure 
// implementations, you'll see that they are about 500-1000 times faster (notice
// the 100000 passed to the timer, vs. 1000 in the other two implementations.)
// Also, these numbers are generally about 10-15% faster than the
// first implementation that used Option[Tree].