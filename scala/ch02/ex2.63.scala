// Compare with ex2.63b.scala, which doesn't use Option[Tree], but instead uses
// case classes (which is a better approach...)

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


def treeToList1 (tree: Option[Tree]): List[Entry] = tree match {
  case None => Nil
  case Some(t) => treeToList1(t.leftBranch) ++ (t.entry :: treeToList1(t.rightBranch))
}

def treeToList2 (tree: Option[Tree]): List[Entry] = {
  def copyToList (tree: Option[Tree], result: List[Entry]): List[Entry] = tree match {
    case None => result
    case Some(t) => copyToList(t.leftBranch, t.entry :: copyToList(t.rightBranch, result))
  }
  copyToList (tree, Nil)
}

val tree1 = Some(Tree(7, 
  Some(Tree(3, Some(Tree(1, None, None)), Some(Tree(5, None, None)))), 
  Some(Tree(9, None, Some(Tree(11, None, None))))))
  
val tree2 = Some(Tree(3, 
  Some(Tree(1, None, None)),
  Some(Tree(7, Some(Tree(5, None, None)), Some(Tree(9, None, Some(Tree(11, None, None))))))))

val tree3 = Some(Tree(5,
  Some(Tree(3, Some(Tree(1, None, None)), None)),
  Some(Tree(9, Some(Tree(7, None, None)), Some(Tree(11, None, None))))))

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
    treeToListProc: (Option[Tree]) => List[Entry],
    tree: Option[Tree]) = {
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
// output:
// starting treeToList1 tree1: 108
// starting treeToList2 tree1: 42
// starting treeToList1 tree2: 32
// starting treeToList2 tree2: 13
// starting treeToList1 tree3: 35
// starting treeToList2 tree3: 16

// If you compare these results to the results for the Scheme and Clojure 
// implementations, you'll see that they are about 500-1000 times faster (notice
// the 100000 passed to the timer, vs. 1000 in the other two implementations.)
