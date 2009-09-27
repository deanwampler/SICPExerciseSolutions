def compose[T1, T2, T3] (f: (T2) => T3, g: (T1) => T2) = (x:T1) => f(g(x))

def repeated[T] (f: (T) => T, n: Int) = {
  def rep(g: (T) => T, i: Int): (T) => T = i match {
    case 1 => (x:T) => g(x)
    case _ => rep(compose(f, g), i-1)
  }
  rep(f, n)
}

def square (n: Int) = n * n

import org.scalatest._
import org.scalatest.matchers._

object repeatedSpec extends Spec with ShouldMatchers {
  describe ("repeated") {
    it ("should compose a function with itself n times") {
      repeated(square, 1)(5) should equal (25)
      repeated(square, 2)(5) should equal (625)
      repeated(square, 2)(2) should equal (16)
      repeated(square, 2)(3) should equal (81)
    }
  }
}
repeatedSpec execute
