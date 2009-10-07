// Church Numerals

// Version 1, closely following the Scheme and Clojure implementations.

type Number = Int                 // Abstract over a "number" type
type F = (Number) => Number       // N maps a number to a number
type N = (F) => F                 // N maps an F to an F

// Effectively, this is the type of a Church Numeral:
type ChurchNumeralType = (F) => Number => Number

// Increment a Church Numeral
def addOne (n: N) = (f:F) => (x:Number) => f(n(f)(x))

val zero = (f:F) => (x:Number) => x

// Work out the definition of "one"
// val one = addOne(zero)
// val one = (f:F) => (x:Number) => f(zero(f)(x))
// val one = (f:F) => (x:Number) => f((g:F) => (y:Number) => y)(f)(x))
// val one = (f:F) => (x:Number) => f((g:F) => (y:Number) => x)(f)(x))
// val one = (f:F) => (x:Number) => f((y:Number) => y)(x)
   val one = (f:F) => (x:Number) => f(x)


// Work out the definition of "two"
// val two = addOne(one)
// val two = (f:F) => (x:Number) => f(one(f)(x))
// val two = (f:F) => (x:Number) => f((g:F) => (y:Number) => g(y))(f)(x))
// val two = (f:F) => (x:Number) => f(f((y:Number) => y)(x))
   val two = (f:F) => (x:Number) => f(f(x))

// The general case (n>=2)
// val n (f:F) => (x:Number) => f( ... f(x))
def churchNumeral (n: Int) = {
  (f:F) => (x:Number) => {
    def g (x: Number, m: Int): Number = m match {
      case 0 => x
      case _ => f(g(x, m-1))
    }
    g(x, n)
  }
}

import org.scalatest._
import org.scalatest.matchers._

object churchNumeralsSpec extends Spec with ShouldMatchers {
  val f1: F = (x:Number) => x+1
  
  describe ("zero") {
    it ("should return the value independent of the function applied to it (i.e., behave like x + 0)") {
      (0 until 5).foreach { i => zero(f1)(i) should equal (i) }
    }
  }
  
  describe ("one") {
    it ("should return the value after applying a function to it once (i.e., behave like x + 1)") {
      (0 until 5).foreach { i => one(f1)(i) should equal (i+1) }
    }
  }
  
  describe ("two") {
    it ("should return the value after applying a function to it twice (i.e., behave like x + 2)") {
      (0 until 5).foreach { i => two(f1)(i) should equal (i+2) }
    }
  }
  
  describe ("addOne") {
    it ("should return the next Church Numeral") {
      (0 until 5).foreach { i => 
        one(f1)(i) should equal (addOne(zero)(f1)(i))
        two(f1)(i) should equal (addOne(one)(f1)(i))
      }
    }
  }
  
  describe ("churchNumeral(n)") {
    it ("""should return the corresponding Church Numeral that returns the value after 
        applying a function to it n times (i.e., behave like x + n)""") {
      for { 
        i <- 0 until 5
        j <- 0 until 5
      } churchNumeral(i)(f1)(j) should equal (i+j)
    }
  }
}

// Second implementation, more idiomatic of Scala, which makes it easier to
// choose different types for Number.

class ChurchNumeral[Number](n: Int) {   // Abstract over a "number" type
  type F = (Number) => Number       // N maps a number to a number
  type N = (F) => F                 // N maps an F to an F

  // Effectively, this is the type of a Church Numeral:
  type ChurchNumeralType = (F) => Number => Number
  
  def apply(f: F)(x: Number) = {
    def g (x: Number, m: Int): Number = m match {
      case 0 => x
      case _ => f(g(x, m-1))
    }
    g(x, n)
  }

  // Increment this Church Numeral
  def addOne = (f:F) => (x:Number) => f(this(f)(x))
}

class  IntChurchNumeral(n: Int) extends ChurchNumeral[Int](n)
object IntChurchNumeral {
  val zero = new IntChurchNumeral(0)
  val one  = new IntChurchNumeral(1)
  val two  = new IntChurchNumeral(2)
}

object intChurchNumeralClassSpec extends Spec with ShouldMatchers {
  val f1 = (x:Int) => x+1
  
  describe ("IntChurchNumeral(n)") {
    it ("""should return the corresponding Church Numeral that returns the value after 
        applying a function to it n times (i.e., behave like x + n)""") {
      for { 
        i <- 0 until 5
        j <- 0 until 5
      } (new IntChurchNumeral(i))(f1)(j) should equal (i+j) 
    }
  }  
  
  describe ("zero") {
    it ("should return the value independent of the function applied to it (i.e., behave like x + 0)") {
      (0 until 5).foreach { i => IntChurchNumeral.zero(f1)(i) should equal (i) }
    }
  }
  
  describe ("one") {
    it ("should return the value after applying a function to it once (i.e., behave like x + 1)") {
      (0 until 5).foreach { i => IntChurchNumeral.one(f1)(i) should equal (i+1) }
    }
  }
  
  describe ("two") {
    it ("should return the value after applying a function to it twice (i.e., behave like x + 2)") {
      (0 until 5).foreach { i => IntChurchNumeral.two(f1)(i) should equal (i+2) }
    }
  }
  
  describe ("addOne") {
    it ("should return the next Church Numeral") {
      (0 until 5).foreach { i => 
        IntChurchNumeral.one(f1)(i) should equal (IntChurchNumeral.zero.addOne(f1)(i))
        IntChurchNumeral.two(f1)(i) should equal (IntChurchNumeral.one.addOne(f1)(i))
      }
    }
  }
}

intChurchNumeralClassSpec execute

// Treat strings as "numbers"!

class  StringChurchNumeral(n: Int) extends ChurchNumeral[String](n)
object StringChurchNumeral {
  val zero = new StringChurchNumeral(0)
  val one  = new StringChurchNumeral(1)
  val two  = new StringChurchNumeral(2)
}

object stringChurchNumeralClassSpec extends Spec with ShouldMatchers {
  val f1 = (x:String) => "{"+x+"}"

  describe ("StringChurchNumeral(n)") {
    it ("""should return the corresponding Church Numeral that returns the value after 
        applying a function to it n times (i.e., behave like x + n)""") {
      StringChurchNumeral.zero(f1)("x") should equal ("x")
      StringChurchNumeral.one(f1)("x")  should equal ("{x}")
      StringChurchNumeral.two(f1)("x")  should equal ("{{x}}")
      (new StringChurchNumeral(3))(f1)("x") should equal ("{{{x}}}")
      (new StringChurchNumeral(4))(f1)("x") should equal ("{{{{x}}}}")
    }
  }  

  describe ("zero") {
    it ("should return the value independent of the function applied to it (i.e., behave like x + 0)") {
      StringChurchNumeral.zero(f1)("x") should equal ("x")
    }
  }

  describe ("one") {
    it ("should return the value after applying a function to it once (i.e., behave like x + 1)") {
      StringChurchNumeral.one(f1)("x")  should equal ("{x}")
    }
  }

  describe ("two") {
    it ("should return the value after applying a function to it twice (i.e., behave like x + 2)") {
      StringChurchNumeral.two(f1)("x")  should equal ("{{x}}")
    }
  }

  describe ("addOne") {
    it ("should return the next Church Numeral") {
      StringChurchNumeral.one(f1)("x") should equal (StringChurchNumeral.zero.addOne(f1)("x"))
      StringChurchNumeral.two(f1)("x") should equal (StringChurchNumeral.one.addOne(f1)("x"))
    }
  }
}

stringChurchNumeralClassSpec execute
  