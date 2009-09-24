def pascalsTriangle(row: Int, col: Int): Int = col match {
  case 1 => 1
  case _ if (col == row) => 1
  case _ => pascalsTriangle(row - 1, col - 1) + pascalsTriangle(row - 1, col)
}

import org.scalatest._
import org.scalatest.matchers._

val ptvals = Map(
  (1,1) -> 1,
  (2,1) -> 1,
  (2,2) -> 1,
  (3,1) -> 1,
  (3,2) -> 2,
  (3,3) -> 1,
  (4,1) -> 1,
  (4,2) -> 3,
  (4,3) -> 3,
  (4,4) -> 1,
  (5,1) -> 1,
  (5,2) -> 4,
  (5,3) -> 6,
  (5,4) -> 4,
  (5,5) -> 1)
  
object pascalsTriangleSpec extends Spec with ShouldMatchers {
  describe ("pascalsTriangle") {
    it ("should calculate the value for a given row and column") {
      ptvals.keys foreach { 
        rowCol => pascalsTriangle(rowCol._1, rowCol._2) should equal (ptvals(rowCol))
      }
    }
  }
}

pascalsTriangleSpec execute
