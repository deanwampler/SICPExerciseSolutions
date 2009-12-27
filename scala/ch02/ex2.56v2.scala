// Ex 2.56 version 2
// This version uses more idiomatic Scala, e.g., objects. Contrast with 
// ex2.56.scala, which follows closely the Scheme and Clojure versions (which
// reflect the book...). 
// This version uses types more extensively. One benefit is the elimination of
// "throws" clauses for invalid cases.

case class Variable(name: String) {
  override def toString = name
}

case class Number(value: Int) {
  override def toString = value.toString
  
  // None of these methods properly handle overflow.
  def +  (other: Number) = new Number(value + other.value)
  def -  (other: Number) = new Number(value - other.value)
  def *  (other: Number) = new Number(value * other.value)
  def ** (other: Number) = new Number(power(value, other.value))
  
  protected def power(base:Int, exp:Int):Int = exp match {
    case 0 => 1
    case 1 => base
    case _ => base * power(base, exp - 1)
  }
}
object Number {
  def apply(s:String) = new Number(Integer.parseInt(s))
}

sealed abstract case class Expression(terms : Expression*) {
  override def toString = terms.toList match {
    case Nil => ""
    case head::Nil => head.toString  // suppress "(...)" for a 1-element list
    case head::tail => 
      String.format(
        "(%s)", (head::tail).map(_.toString).reduceLeft(_ + " " + _))
  }
}

sealed abstract class Operator(val symbol:String) extends Expression {
  override def toString = symbol 
  override def equals(other: Any) = other match {
    case o:Operator => symbol == o.symbol
    case _ => false
  }
}

sealed abstract class Term extends Expression

case class VariableExpression(variable: Variable) extends Term {
  override def toString = variable.toString
}
object VariableExpression {
  def apply(s: String) = new VariableExpression(Variable(s))
}

case class NumberExpression(number: Number) extends Term {
  override def toString = number.toString
}
object NumberExpression {
  def apply(i: Int) = new NumberExpression(Number(i))
}

abstract class ArithmeticExpression(
  val operator: Operator, 
  val leftOperand: Expression, 
  val rightOperand: Expression) 
    extends Expression(operator, leftOperand, rightOperand) 

// We don't use case classes for the arithmetic expressions so we can 
// define our own apply methods. Unfortunately, this means reproducing other
// boilerplate, like conventional unapply methods.

class Sum(val addend: Expression, val augend: Expression)
    extends ArithmeticExpression(Sum.Plus, addend, augend) {
}
object Sum {
  case object Plus extends Operator("+") 

  def apply(leftOp: Expression, rightOp: Expression) = leftOp match {
    case ZeroExpr => rightOp
    case n1:NumberExpression => rightOp match {
      case ZeroExpr => leftOp
      case n2:NumberExpression => NumberExpression(n1.number + n2.number)
      case _ => new Sum(leftOp, rightOp)
    }
    case _ => new Sum(leftOp, rightOp)
  }

  def unapply(sum: Sum) = Some((sum.addend, sum.augend))
}

class Difference(val minuend: Expression, val subtrahend: Expression) 
    extends ArithmeticExpression(Difference.Minus, minuend, subtrahend) {
}
object Difference {
  case object Minus extends Operator("-") 

  def apply(leftOp: Expression, rightOp: Expression) = leftOp match {
    case ZeroExpr => new Difference(leftOp, rightOp)
    case n1:NumberExpression => rightOp match {
      case ZeroExpr => leftOp
      case n2:NumberExpression => NumberExpression(n1.number - n2.number)
      case _ => new Difference(leftOp, rightOp)
    }
    case _ => new Difference(leftOp, rightOp)
  }

  def unapply(diff: Difference) = Some((diff.minuend, diff.subtrahend))
}

class Product(val multiplier: Expression, val multiplicand: Expression) 
    extends ArithmeticExpression(Product.Times, multiplier, multiplicand) {
}
object Product {
  case object Times extends Operator("*") 

  def apply(leftOp: Expression, rightOp: Expression) = 
    if      (leftOp == ZeroExpr || rightOp == ZeroExpr) ZeroExpr
    else if (leftOp == OneExpr)  rightOp
    else if (rightOp == OneExpr) leftOp
    else {
      leftOp match {
        case n1:NumberExpression => rightOp match {
          case n2:NumberExpression => NumberExpression(n1.number * n2.number)
          case _ => new Product(leftOp, rightOp)
        }
        case _ => new Product(leftOp, rightOp)
      }
    }

  def unapply(prod: Product) = Some((prod.multiplier, prod.multiplicand))
}

class Exponentiation(val base: Expression, val exponent: Expression)
  extends ArithmeticExpression(Exponentiation.Exponent, base, exponent) {
}
object Exponentiation {
  case object Exponent extends Operator("**") 

  def apply(leftOp: Expression, rightOp: Expression) = 
    if      (leftOp == ZeroExpr) ZeroExpr
    else if (rightOp == ZeroExpr) OneExpr
    else if (rightOp == OneExpr)  leftOp
    else {
      leftOp match {
        case n1:NumberExpression => rightOp match {
          case n2:NumberExpression => NumberExpression(n1.number ** n2.number)
          case _ => new Exponentiation(leftOp, rightOp)
        }
        case _ => new Exponentiation(leftOp, rightOp)
      }
    }

  def unapply(ex: Exponentiation) = Some((ex.base, ex.exponent))
}

val Zero = Number(0)
val One  = Number(1)
val ZeroExpr = NumberExpression(Zero)
val OneExpr  = NumberExpression(One)

// We use the parser combinator library to parse the expression strings into 
// Expression objects. 
import scala.util.parsing.combinator._

// "wholeNumber" and "ident" supplied by JavaTokenParsers
object expressionParser extends JavaTokenParsers {
  def expression = arithmeticExpression | number | variable
  def arithmeticExpression: Parser[Expression] = "(" ~> operator ~ operand ~ operand <~ ")" ^^ {
    case Sum.Plus                ~ o1 ~ o2 => Sum(o1, o2)
    case Difference.Minus        ~ o1 ~ o2 => Difference(o1, o2)
    case Product.Times           ~ o1 ~ o2 => Product(o1, o2)
    case Exponentiation.Exponent ~ o1 ~ o2 => Exponentiation(o1, o2)
  }
  def operand = (arithmeticExpression | number | variable)
  def number = wholeNumber ^^ { n => NumberExpression(Integer.parseInt(n)) }
  def variable = ident ^^ { v => VariableExpression(v) }
  def operator = sum | difference | exponentiation | product
  def sum            = "+"  ^^ { _ => Sum.Plus }
  def difference     = "-"  ^^ { _ => Difference.Minus }
  def exponentiation = "**" ^^ { _ => Exponentiation.Exponent }
  def product        = "*"  ^^ { _ => Product.Times }
}

implicit def expressionStringToExpression(exp: String): Expression = 
  expressionParser.parseAll(expressionParser.expression, exp) match {
    case expressionParser.Success(e,_) => e
    case x => throw new RuntimeException("expression parsing failed! "+x)
  }
  
implicit def expressionStringToVariable(exp: String): Variable = Variable(exp)

object Deriv {
  def apply (exp: Expression, variable: Variable): Expression = exp match {
    case NumberExpression(n) => ZeroExpr
    case VariableExpression(v) => if (v == variable) OneExpr else ZeroExpr
    case Sum(addend, augend) => Sum(Deriv(addend, variable), Deriv(augend, variable))
    case Difference(minuend, subtrahend) =>
      Difference(Deriv(minuend, variable), Deriv(subtrahend, variable))
    case Product(multiplier, multiplicand) => 
      Sum(Product(multiplier, Deriv(multiplicand, variable)), 
          Product(Deriv(multiplier, variable), multiplicand))
    case Exponentiation(base, exponent) => 
      Product(Product(exponent, Exponentiation(base, Difference(exponent, OneExpr))), 
              Deriv(base, variable))
    case _ => 
      throw new RuntimeException("unknown expression type -- DERIV: "+exp)
  }
}

import org.scalatest._ 
import org.scalatest.matchers._

object derivSpec extends Spec with ShouldMatchers {
  implicit def intToNumberExpression(i: Int) = new NumberExpression(Number(i))
  
  describe ("expressionParser") {
    it ("should parse expression strings into list trees") {
      expressionStringToExpression("0")  should equal (ZeroExpr)
      expressionStringToExpression("1")  should equal (OneExpr)
      expressionStringToExpression("x")  should equal (VariableExpression("x"))
      expressionStringToExpression("(+  1 2)")  should equal (NumberExpression(3))
      expressionStringToExpression("(-  1 2)")  should equal (NumberExpression(-1))
      expressionStringToExpression("(*  2 3)")  should equal (NumberExpression(6))
      expressionStringToExpression("(** 2 3)")  should equal (NumberExpression(8))
      expressionStringToExpression("(+  x 3)")  should equal (Sum("x", 3))
      expressionStringToExpression("(-  x 3)")  should equal (Difference("x", 3))
      expressionStringToExpression("(*  x 3)")  should equal (Product("x", 3))
      expressionStringToExpression("(** x 3)")  should equal (Exponentiation("x", 3))
      expressionStringToExpression("(* (* x y) (- x 3))") should equal (
        Product(Product("x", "y"), Difference("x", 3)))
    }
  }
  describe ("Deriv") {
    it ("should compute the correct differentation expressions") {
      Deriv ("(+ x 3)", "x").toString should equal ("1")
      Deriv ("(- x 3)", "x").toString should equal ("1")
      Deriv ("(* x y)", "x").toString should equal ("y")
      Deriv ("(* (* x y) (+ x 3))", "x").toString should equal ("(+ (* x y) (* y (+ x 3)))")
      Deriv ("(* (* x y) (- x 3))", "x").toString should equal ("(+ (* x y) (* y (- x 3)))")
      
      Deriv ("(** x 1)", "x").toString should equal ("1")
      Deriv ("(** x 2)", "x").toString should equal ("(* 2 x)")
      Deriv ("(** x 3)", "x").toString should equal ("(* 3 (** x 2))")
      Deriv ("(** x 4)", "x").toString should equal ("(* 4 (** x 3))")
      Deriv ("(** x n)", "x").toString should equal ("(* n (** x (- n 1)))")
    }
  }
}
derivSpec execute