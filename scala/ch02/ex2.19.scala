def reverse[A](l: List[A]) = {
  def rev(l2: List[A], result: List[A]): List[A] = l2 match {
    case Nil => result
    case _ => rev(l2.tail, l2.head :: result)
  }
  rev(l, Nil)
}

def firstDenomination (coinValues: List[Double]) = coinValues.head
def exceptFirstDenomination (coinValues: List[Double]) = coinValues.tail
def noMore (coinValues: List[Double]) = coinValues == Nil

def cc (amount: Double, coinValues: List[Double]): Double = amount match {
  case 0 => 1
  case _ if (amount < 0 || noMore(coinValues)) => 0
  case _ =>
    cc(amount, exceptFirstDenomination(coinValues)) +
    cc(amount - firstDenomination(coinValues), coinValues)
  }

import org.scalatest._ 
import org.scalatest.matchers._

object countCoinsSpec extends Spec with ShouldMatchers {
  describe ("cc") {
    it ("should the correct count independent of the order of the list of coins") {
      val usCoins = List[Double](50, 25, 10, 5, 1)
      val ukCoins = List[Double](100, 50, 20, 10, 5, 2, 1, 0.5)

      // The order of the coins doesn't matter:
      cc(100, ukCoins) should equal(104561)
      cc(100, usCoins) should equal(292)
      cc(100, reverse(usCoins)) should equal(292)
      cc(100, List(25, 10, 5, 1, 50)) should equal(292)
      cc(100, List(10, 5, 1, 50, 25)) should equal(292)
      cc(100, List(5, 1, 50, 25, 10)) should equal(292)
      cc(100, List(1, 50, 25, 10, 5)) should equal(292)
    }
  }
} 

countCoinsSpec execute
