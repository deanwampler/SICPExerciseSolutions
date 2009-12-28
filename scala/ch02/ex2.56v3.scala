// Ex 2.56 version 2
// This version uses more idiomatic Scala, e.g., objects. Contrast with 
// ex2.56.scala, which follows closely the Scheme and Clojure versions (which
// reflect the book...). 
// This version uses types more extensively. One benefit is the reduction of
// "throws" clauses for invalid cases, most of which can be eliminated.


sealed abstract class Expression

case class Variable(name: String) extends Expression {
  override def toString = name
}

case class Number(value: Int) extends Expression {
  override def toString = value.toString
  
  // None of these methods properly handle overflow.
  def +  (other: Number) = new Number(value + other.value)
  def -  (other: Number) = new Number(value - other.value)
  def *  (other: Number) = new Number(value * other.value)
  def ** (other: Number) = new Number(power(value, other.value))
  def unary_- = new Number(-value)
  
  protected def power(base:Int, exp:Int):Int = exp match {
    case 0 => 1
    case 1 => base
    case _ => base * power(base, exp - 1)
  }
}

implicit def intToNumber(i: Int) = Number(i)

// We can't use a case class here because we need to define our own apply method
// in the companion object.

class ArithmeticExpression(
  val operator: Operator, 
  val leftOperand: Expression, 
  val rightOperand: Expression) extends Expression{

  override def toString = 
    String.format("(%s %s %s)", operator, leftOperand, rightOperand)

  override def equals(other: Any) = other match {
    case ArithmeticExpression(op, left, right) =>
      op == operator && left == leftOperand && right == rightOperand
    case _ => false
  }

  override def hashCode = 
    37 * (operator.hashCode + leftOperand.hashCode + rightOperand.hashCode)
}
object ArithmeticExpression {
  def apply(op: Operator, leftOp: Expression, rightOp: Expression):Expression = 
    op(leftOp, rightOp)

  def unapply(ae: ArithmeticExpression) = Some((ae.operator, ae.leftOperand, ae.rightOperand))
}


sealed abstract class Operator(val symbol:String) extends Expression {
  override def toString = symbol 
  def apply(leftOp: Expression, rightOp: Expression): Expression
}

case object Plus extends Operator("+") {
  def apply(addend: Expression, augend: Expression) = addend match {
    case Zero => augend
    case n1:Number => augend match {
      case Zero => addend
      case n2:Number => n1 + n2
      case _ => new ArithmeticExpression(Plus, addend, augend)
    }
    case _ => new ArithmeticExpression(Plus, addend, augend)
  }
}

case object Minus extends Operator("-") {
  def apply(minuend: Expression, subtrahend: Expression) = minuend match {
    case Zero => subtrahend match {
      case n2:Number => -n2
      case _ => new ArithmeticExpression(Minus, Zero, subtrahend) // TODO, handle negative expressions properly.
    }
    case n1:Number => subtrahend match {
      case Zero => minuend
      case n2:Number => n1 - n2
      case _ => new ArithmeticExpression(Minus, minuend, subtrahend)
    }
    case _ => new ArithmeticExpression(Minus, minuend, subtrahend)
  }
}

case object Times extends Operator("*") {
  def apply(multiplier: Expression, multiplicand: Expression) = 
    if      (multiplier == Zero || multiplicand == Zero) Zero
    else if (multiplier == One) multiplicand
    else if (multiplicand == One) multiplier
    else {
      multiplier match {
        case n1:Number => multiplicand match {
          case n2:Number => n1 * n2
          case _ => new ArithmeticExpression(Times, multiplier, multiplicand)
        }
        case _ => new ArithmeticExpression(Times, multiplier, multiplicand)
      }
    }
}

case object Exponent extends Operator("**") {
  def apply(base: Expression, exponent: Expression) = 
    if      (base     == Zero) Zero
    else if (exponent == Zero) One
    else if (exponent == One)  base
    else {
      base match {
        case n1:Number => exponent match {
          case n2:Number => n1 ** n2
          case _ => new ArithmeticExpression(Exponent, base, exponent)
        }
        case _ => new ArithmeticExpression(Exponent, base, exponent)
      }
    }
}

// Helper classes and implicits to enable algebraic expressions, 
// e.g., ex1 + ex2 instead of Sum(ex1, ex2).
object ArithmeticDSL {
  class WithOperators(val exp1: Expression) {
    def +  (exp2: Expression) = ArithmeticExpression(Plus, exp1, exp2)
    def -  (exp2: Expression) = ArithmeticExpression(Minus, exp1, exp2)
    def *  (exp2: Expression) = ArithmeticExpression(Times, exp1, exp2)
    def ** (exp2: Expression) = ArithmeticExpression(Exponent, exp1, exp2)
  }

  implicit def expressionToWithOperators(e: Expression) = new WithOperators(e)
}
import ArithmeticDSL._

// We use the parser combinator library to parse the expression strings into 
// Expression objects. 
import scala.util.parsing.combinator._

// "wholeNumber" and "ident" supplied by JavaTokenParsers
object expressionParser extends JavaTokenParsers {
  def expression = arithmeticExpression | number | variable
  def arithmeticExpression: Parser[Expression] = "(" ~> operator ~ operand ~ operand <~ ")" ^^ {
    case op ~ o1 ~ o2 => ArithmeticExpression(op, o1, o2)
  }
  def operand  = (arithmeticExpression | number | variable)
  def number   = wholeNumber ^^ { n => Number(Integer.parseInt(n)) }
  def variable = ident       ^^ { v => Variable(v) }
  def operator = sum | difference | exponentiation | product
  def sum            = "+"  ^^ { _ => Plus }
  def difference     = "-"  ^^ { _ => Minus }
  def exponentiation = "**" ^^ { _ => Exponent }
  def product        = "*"  ^^ { _ => Times }
}

val Zero = Number(0)
val One  = Number(1)

object Deriv {
  
  class ParseError(message: String) extends RuntimeException(message)
  
  def apply (exp: Expression, variable: Variable): Expression = exp match {
    case n:Number => Zero
    case v:Variable => if (v == variable) One else Zero
    case ArithmeticExpression(operator, leftOp, rightOp) => operator match {
      case Plus  => Deriv(leftOp, variable) + Deriv(rightOp, variable)
      case Minus => Deriv(leftOp, variable) - Deriv(rightOp, variable)
      case Times => 
        (leftOp * Deriv(rightOp, variable)) + 
        (Deriv(leftOp, variable) * rightOp)
      case Exponent =>
        rightOp * (leftOp ** (rightOp - One)) * Deriv(leftOp, variable)
    }
    case o:Operator => throw new ParseError("bare operator found: "+o)
  }
  
  def apply (exp: String, variable: String): Expression =
    apply(expressionStringToExpression(exp), Variable(variable))

  implicit def expressionStringToExpression(exp: String): Expression = 
    expressionParser.parseAll(expressionParser.expression, exp) match {
      case expressionParser.Success(e,_) => e
      case x => throw new ParseError("expression parse failure! "+x)
    }
}

import Deriv.expressionStringToExpression

import org.scalatest._ 
import org.scalatest.matchers._

object derivSpec extends Spec with ShouldMatchers {
  
  describe ("expressionParser") {
    it ("should parse expression strings into list trees") {
      expressionStringToExpression("0")  should equal (Zero)
      expressionStringToExpression("1")  should equal (One)
      expressionStringToExpression("x")  should equal (Variable("x"))
      expressionStringToExpression("(+  0 2)")  should equal (Number(2))
      expressionStringToExpression("(+  2 0)")  should equal (Number(2))
      expressionStringToExpression("(+  1 2)")  should equal (Number(3))
      expressionStringToExpression("(-  0 2)")  should equal (Number(-2))
      expressionStringToExpression("(-  2 0)")  should equal (Number(2))
      expressionStringToExpression("(-  1 2)")  should equal (Number(-1))
      expressionStringToExpression("(*  2 0)")  should equal (Number(0))
      expressionStringToExpression("(*  0 2)")  should equal (Number(0))
      expressionStringToExpression("(*  2 3)")  should equal (Number(6))
      expressionStringToExpression("(** 0 3)")  should equal (Number(0))
      expressionStringToExpression("(** 3 0)")  should equal (Number(1))
      expressionStringToExpression("(** 2 3)")  should equal (Number(8))
      expressionStringToExpression("(+  x 3)")  should equal (ArithmeticExpression(Plus, Variable("x"), Number(3)))
      expressionStringToExpression("(+  3 x)")  should equal (ArithmeticExpression(Plus, 3, "x"))
      expressionStringToExpression("(-  x 3)")  should equal (ArithmeticExpression(Minus, "x", 3))
      expressionStringToExpression("(-  3 x)")  should equal (ArithmeticExpression(Minus, 3, "x"))
      expressionStringToExpression("(*  x 3)")  should equal (ArithmeticExpression(Times, "x", 3))
      expressionStringToExpression("(*  3 x)")  should equal (ArithmeticExpression(Times, 3, "x"))
      expressionStringToExpression("(** x 3)")  should equal (ArithmeticExpression(Exponent, "x", 3))
      expressionStringToExpression("(** 3 x)")  should equal (ArithmeticExpression(Exponent, 3, "x"))
      expressionStringToExpression("(* (* x y) (- x 3))") should equal (
        ArithmeticExpression(Times, ArithmeticExpression(Times, "x", "y"), ArithmeticExpression(Minus, "x", 3)))
    }
  }
  describe ("Deriv") {
    it ("should compute the correct differentation expressions") {
      Deriv ("(* x y)", "x").toString should equal ("y")
      Deriv ("(+ x 3)", "x").toString should equal ("1")
      Deriv ("(- x 3)", "x").toString should equal ("1")
      Deriv ("(- 3 x)", "x").toString should equal ("-1")
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