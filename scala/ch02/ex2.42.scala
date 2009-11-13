
def enumerateInterval (start: Int, end: Int) = {
  def iter (result:List[Int], n:Int): List[Int] = 
    if (n > end)
      result
    else
      iter (result ++ List(n), n + 1)
  iter(Nil, start)
}

type Position = List[Int]

val emptyBoard:List[Position] = List(Nil)

// Represent each position as a list of rows. The position in
// the list is the column, in reverse, with both rows and columns counting from 1.
def adjoinPosition (newRow: Int, col:Int, restOfQueens:List[Position]) =
  restOfQueens map ((position: Position) => newRow :: position)

// By definition, this is the only queen in the k'th column, so we just
// check rows and diagonals for previous columns.
def safe (k:Int, position: List[Position]) = {
  val p = position.head
  val newRow = position.head.head
  def checkRows (pos: Position): Boolean = pos match {
    case Nil => true
    case _ if newRow == pos.head => false
    case _ => checkRows (pos.tail)
  }
  def checkDiags (n:Int, pos: Position): Boolean = pos match {
    case Nil => true
    case _ if (pos.head == newRow + n) || (pos.head == newRow - n) => false
    case _ => checkDiags (n+1, pos.tail)
  }
  checkRows (p.tail) && checkDiags (1, p.tail)
}

def queens (boardSize: Int) = {
  def queenCols (k: Int):List[List[Position]] = 
    if (k == 0)
      List(emptyBoard)
    else
      // use Scala's flatMap now.
      queenCols (k - 1) flatMap {restOfQueens: List[Position] =>
        enumerateInterval(1, boardSize) map {
          newRow:Int => adjoinPosition (newRow, k, restOfQueens)
        }
      } filter { position: List[Position] => safe (k, position) }

  queenCols (boardSize)
}

import org.scalatest._ 
import org.scalatest.matchers._

object queensSpec extends Spec with ShouldMatchers {
  def testQueen (n: Int, expectedN: Int) = {
    val queensN = queens(n)
    format("board size = %d, # solutions = %d: %s\n", n, queensN.size, queensN)
    queensN.size should equal (expectedN)
  }
  
  describe ("queens") {
    it ("should return the correct number of correct answers for a given board size") {
      Map(0 -> 1, 1 -> 1, 2 -> 0, 3 -> 0, 4 -> 2, 
          5 -> 10, 6 -> 4, 7 -> 40, 8 -> 92).foreach { (ne:Pair[Int,Int]) =>
        testQueen (ne._1, ne._2)
      }
    }
  }
}
queensSpec execute