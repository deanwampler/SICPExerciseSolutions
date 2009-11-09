// To keep the solution close to scheme and clojure version, I'll use an Either
// to allow a branch to have either a terminal weight or a "submobile". An alternative
// would be to have a uniform "end" type with subtypes that represent the two
// cases.
// Would prefer AnyRef -> Mobile, but that creates a disallowed cylic reference. 
type Structure = Either[Int, Pair[_,_]]  
type Branch = Pair[Int, Structure]
type Mobile = Pair[Branch,Branch]

def makeMobile (left: Branch, right: Branch): Mobile = (left, right)

def makeBranch (length: Int, structure: Int): Branch = (length, Left(structure)) 
def makeBranch (length: Int, structure: Mobile): Branch = (length, Right(structure)) 

def leftBranch (m: Mobile) = m._1

def rightBranch (m: Mobile) = m._2
  
def branchLength (b: Branch) = b._1

def branchStructure (b: Branch) = b._2
  
def totalWeight (m: Mobile): Int = {
  def evalBranch (b: Branch) = branchStructure(b) fold (
    (i:Int) => i, 
    (p: Pair[_,_]) => totalWeight(p.asInstanceOf[Mobile]))

  evalBranch (leftBranch(m)) + evalBranch (rightBranch(m))
}
        
val m1   = makeMobile (makeBranch (2, 5),  makeBranch (1,  10))
val m2   = makeMobile (makeBranch (3, 4),  makeBranch (4,  3))
val m12  = makeMobile (makeBranch (5, m1), makeBranch (6,  m2))
val m12b = makeMobile (makeBranch (7, m1), makeBranch (15, m2))

import org.scalatest._ 
import org.scalatest.matchers._

object mobileTotalWeightSpec extends Spec with ShouldMatchers {
  describe ("totalWeight") {
    it ("should compute the total weight of the input mobile") {
      totalWeight (m1)   should equal (15)
      totalWeight (m2)   should equal (7)
      totalWeight (m12)  should equal (22)
      totalWeight (m12b) should equal (22)
    }
  }
}
mobileTotalWeightSpec execute

def isBalanced (m: Mobile): Boolean = {
  def branchBalance (b: Branch): Either[Boolean,Int] = {
    val struct = branchStructure (b)
    val len = branchLength (b)
    struct fold ((i:Int) => Right(len * i), 
                (p:Pair[_,_]) => {
                  val s = p.asInstanceOf[Mobile]
                  if (isBalanced(s)) Right(len * totalWeight(s)) else Left(false)})
  }
  def balanceBoolean(b: Either[Boolean,Int]) = (b.isLeft && b.left.get) || true

  val lBalance = branchBalance (leftBranch(m))
  val rBalance = branchBalance (rightBranch(m))
  
  if (! balanceBoolean(lBalance) || ! balanceBoolean(rBalance)) 
    false 
  else 
    lBalance.right.get - rBalance.right.get == 0
}

object mobileIsBalancedSpec extends Spec with ShouldMatchers {
  describe ("isBalanced") {
    it ("should return true if the input mobile is balanced") {
      isBalanced (m1)   should equal (true)
      isBalanced (m2)   should equal (true)
      isBalanced (m12)  should equal (false)
      isBalanced (m12b) should equal (true)
    }
  }
}
mobileIsBalancedSpec execute
