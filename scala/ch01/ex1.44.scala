def smooth (f: (Double) => Double, dx: Double) =
  (x:Double) => (f(x - dx) + f(x) + f(x + dx)) / 3.0

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

object smoothSpec extends Spec with ShouldMatchers {
  describe ("smooth") {
    it ("should smooth the value for f(x)") {
      smooth((x:Double) => 3*x, 0.5)(2.0) should equal (6.0)
    }
  }
}
smoothSpec execute

val piOver4 = 3.14159265 / 4
val dx = 0.005

Math.sin(piOver4)
repeated (smooth (Math.sin, dx), 1)(piOver4)
repeated (smooth (Math.sin, dx), 2)(piOver4)
repeated (smooth (Math.sin, dx), 4)(piOver4)
