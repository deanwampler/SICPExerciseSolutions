// Adapts Ex 2.57. One key change. Before, the first operand was "list.tail.head" of
// the expression and the operator was the "list.head". Now the operator is removed 
// before calling "addend", etc., so the first operand is "list.head" and the rest of
// them are "list.tail".

type Expression = List[Any]
type Variable = String
type Number = Int
val Zero = 0
val One  = 1

private def toExpr(tokens: Any*) = tokens.size match {
  case 1 => tokens(0) match {
    case l:List[_] => l
    case x => List(x)
  }
  case _ => tokens.toList
}

private val ZeroExpr = toExpr(Zero)
private val OneExpr  = toExpr(One)

def isNumber (exp: Expression) = exp match {
  case head :: Nil => head match {
    case s:String => s matches """-?[\d\.]+"""
    case i:Int => true
    case _ => false
  }
  case _ => false
}

// Assumes isNumber returns true
def toNumber (exp: Expression) = exp.head match {
  case s:String => Integer.parseInt(s)
  case i:Int => i
  case x => throw new RuntimeException("exp.head is not a string: "+x.toString)
}

def numberEq (exp: Expression, num: Number) =
  isNumber(exp) && toNumber(exp) == num
  
def isVariable (exp: Expression) = exp match {
  case head :: Nil => head match {
    case s:String => s matches """\D\w*"""
    case _ => false
  }
  case _ => false
}

// Assumes isVariable returns true
def toVariable (exp: Expression) = exp.head match {
  case s:String => s
  case x => throw new RuntimeException("exp.head is not a string: "+x.toString)
}
  
def isSameVariable (v1: Variable, v2: Variable) = v1 == v2
  
// Assumes the expression contains at least one argument/operand
private def extractFirstArg (exp: Expression) = toExpr(exp.head)

// Assumes the expression contains at least two arguments/operands
private def extractRestOfArgs(
    exp: Expression, op: (Expression, Expression) => Expression) = {
  def rest(exp2: Expression, total: Expression): Expression = exp2 match {
    case Nil => total
    case head::tail => rest(tail, op(total, toExpr(head)))
  }
  rest(exp.tail.tail, toExpr(exp.tail.head))
}
  
def addend (exp: Expression) = extractFirstArg(exp)
def augend (exp: Expression) = extractRestOfArgs(exp, makeSum)

def makeSum (exp1: Expression, exp2: Expression) =
  if      (numberEq(exp1, Zero)) exp2
  else if (numberEq(exp2, Zero)) exp1
  else if (isNumber(exp1) && isNumber(exp2)) 
    toExpr(toNumber(exp1) + toNumber(exp2))
  else 
    toExpr("+", exp1, exp2)
    
def minuend (exp: Expression) = extractFirstArg(exp)
// (- x1 x2 x3 ...) => (- x1 (+ x2 x3 ...))
def subtrahend (exp: Expression) = extractRestOfArgs(exp, makeSum)
  
def makeDifference (exp1: Expression, exp2: Expression) =
  if      (isNumber(exp1) && isNumber(exp2)) 
    toExpr(toNumber(exp1) - toNumber(exp2))
  else if (numberEq(exp1, Zero))
    toExpr("-", Zero, exp2)
  else if (numberEq(exp2, Zero)) exp1
  else 
    toExpr("-", exp1, exp2)
  
def multiplier (exp: Expression) = extractFirstArg(exp)  
def multiplicand (exp: Expression) = extractRestOfArgs(exp, makeProduct)
  
def makeProduct (exp1: Expression, exp2: Expression) =
  if      (numberEq(exp1, Zero) || numberEq(exp2, Zero)) ZeroExpr
  else if (numberEq(exp1, One)) exp2
  else if (numberEq(exp2, One)) exp1
  else if (isNumber(exp1) && isNumber(exp2))
    toExpr(toNumber(exp1) * toNumber(exp2))
  else 
    toExpr("*", exp1, exp2)

def base (exp: Expression) = extractFirstArg(exp)  
// Exponents are multipled together.
def exponent (exp: Expression) = extractRestOfArgs(exp, makeProduct)
  
private def calcNumberExponential(base: Number, exponent: Number): Number =
  exponent match {
    case Zero => One
    case _ => base * calcNumberExponential(base, exponent - 1)
  }
  
def makeExponentiation (exp1: Expression, exp2: Expression): Expression =
  if      (numberEq(exp1, Zero)) ZeroExpr
  else if (numberEq(exp2, Zero)) OneExpr
  else if (numberEq(exp2, One))  exp1
  else if (isNumber(exp1) && isNumber(exp2))
    toExpr(calcNumberExponential(toNumber(exp1), toNumber(exp2)))
  else 
    toExpr("**", exp1, exp2)

// We use the parser combinator library to parse the expression strings into 
// Expression objects. 
// Actually, if we're doing this, we could just do the differentiation 
// as we go. However, it's usually better to separate parsing of expressions 
// from using 'em...
import scala.util.parsing.combinator._

// "wholeNumber" and "ident" supplied by JavaTokenParsers
object expressionParser extends JavaTokenParsers {
  def expression = parentheticalExpression | tokenAsList
  def exp: Parser[Any] = parentheticalExpression | token
  def parentheticalExpression = "(" ~> operator ~ operands <~ ")" ^^ { 
    case op ~ rands => op :: rands
  }
  def operands = exp ~ rep1(exp) ^^ { 
    case e ~ es => e :: es
  }
  def tokenAsList = token ^^ { t => List(t) }
  def token = ( number | variable )
  def number = wholeNumber ^^ { n => Integer.parseInt(n) }  
  def variable = ident
  def operator = sum | difference | exponentiation | product
  def sum = "+"
  def difference = "-"
  def exponentiation = "**"
  def product = "*"
}

implicit def expressionStringToExpression(exp: String): Expression = 
  expressionParser.parseAll(expressionParser.expression, exp) match {
    case expressionParser.Success(e,_) => e
    case x => throw new RuntimeException("expression parsing failed! "+x)
  }
  
// Convert an expression back into an sexp string.
private def stringize(exp: Any): String = exp match {
  case Nil => ""
  case head::Nil => stringize(head)  // suppress "(...)" for a 1-element list
  case head::tail => 
    String.format(
      "(%s)", (head::tail).map(stringize(_)).reduceLeft(_ + " " + _))
  case x => x.toString
}

def operator(exp: Expression):String = exp.head match {
  case s:String => s
  case x => throw new RuntimeException("Expected "+x+" to be a string for an operator.")
}
def operands(exp: Expression):Expression = exp.tail
          
def deriv (exp: Expression, variable: Variable): String = 
  stringize(computeDeriv(exp, variable))

private def computeDeriv (exp: Expression, variable: Variable): Expression =
  if      (isNumber(exp)) ZeroExpr
  else if (isVariable(exp))
    if (isSameVariable(toVariable(exp), variable)) OneExpr else ZeroExpr
  else get("deriv", operator(exp))(operands(exp), variable)

// a. All the operators have uniform behavior; they are applied to a list of
// operands, which are numbers, variables, or arithmetic combinations of them
// with operators. Numbers and variables are "leaf nodes" in expression trees,
// requiring unique handling.
//
// b. and c. procedures for sums, differences, products, and exponentials.
// Since we don't yet have "get" and "put" (until 3.3.3), we'll use a mutable HashMap
// to do what we want. We'll ignore the table key "deriv" and just worry about
// the operator.

var table = 
  new scala.collection.mutable.HashMap[String, (Expression, String) => Expression]()

def get (ignore: String, op: String) = table.get(op) match {
  case None => throw new RuntimeException("No match found for " + op)
  case Some(proc) => proc
}
  
def put (key: String, tableKey: String, proc: (Expression, String) => Expression) =
  table += Tuple2(key, proc)

def attachTag (typeTag: String, contents: Expression) = typeTag :: contents
val typeTag = (exp: Expression) => exp.head
val contents = (exp: Expression) => exp.tail
      
def installDerivPackage = {
  val sum = (operands: Expression, variable: String) =>
    makeSum (deriv (addend(operands), variable),
             deriv (augend(operands), variable))
  val difference = (operands: Expression, variable: String) =>
    makeDifference (deriv (minuend(operands), variable),
                    deriv (subtrahend(operands), variable))
  val product = (operands: Expression, variable: String) =>
    makeSum(makeProduct (multiplier(operands),
                         deriv (multiplicand(operands), variable)),
            makeProduct (deriv (multiplier(operands), variable),
                         multiplicand(operands)))
  val exponentiation = (operands: Expression, variable: String) =>
    makeProduct(makeProduct (exponent(operands),
                             makeExponentiation (base(operands), 
                                                 makeDifference (exponent(operands), OneExpr))),
                makeProduct (deriv (base(operands), variable), OneExpr))

  def tag (x: Expression) = attachTag ("deriv", x)
  put ("+",  "deriv", sum)
  put ("-",  "deriv", difference)
  put ("*",  "deriv", product)
  put ("**", "deriv", exponentiation)
}
installDerivPackage


import org.scalatest._ 
import org.scalatest.matchers._

object derivSpec extends Spec with ShouldMatchers {
  describe ("expressionParser") {
    it ("should parse expression strings into list trees") {
      expressionStringToExpression("1")  should equal (List(1))
      expressionStringToExpression("x")  should equal (List("x"))
      expressionStringToExpression("(+  1 2)")  should equal (List("+",  1, 2))
      expressionStringToExpression("(+  1 2 3 4)")  should equal (List("+",  1, 2, 3, 4))
      expressionStringToExpression("(-  1 2)")  should equal (List("-",  1, 2))
      expressionStringToExpression("(-  1 2 3 4)")  should equal (List("-",  1, 2, 3, 4))
      expressionStringToExpression("(*  1 2)")  should equal (List("*",  1, 2))
      expressionStringToExpression("(*  1 2 3 4)")  should equal (List("*",  1, 2, 3, 4))
      expressionStringToExpression("(** 1 2)")  should equal (List("**", 1, 2))
      expressionStringToExpression("(** 1 2 3 4)")  should equal (List("**", 1, 2, 3, 4))
      expressionStringToExpression("(+ x 3)") should equal (List("+", "x", 3))
      expressionStringToExpression("(+ x y z 3)") should equal (List("+", "x", "y", "z", 3))
      expressionStringToExpression("(* (* x y) (- x 3))") should equal (
        List("*", List("*", "x", "y"), List("-", "x", 3)))
      expressionStringToExpression("(* x y (- x 3))") should equal (
        List("*", "x", "y", List("-", "x", 3)))
    }
  }
  describe ("deriv") {
    it ("should compute the correct differentation expressions") {
      deriv ("(+ x 3)", "x") should equal ("1")
      deriv ("(+ x x 3)", "x") should equal ("2")
      deriv ("(- x 3)", "x") should equal ("1")
      deriv ("(- x x 3)", "x") should equal ("0")
      deriv ("(- x x x x 3)", "x") should equal ("-2")
      deriv ("(* x y)", "x") should equal ("y")
      deriv ("(* x x y)", "x") should equal ("(+ (* x y) (* x y))")
      deriv ("(* (* x y) (+ x 3))", "x") should equal ("(+ (* x y) (* y (+ x 3)))")
      deriv ("(* x y (+ x 3))", "x") should equal ("(+ (* x y) (* y (+ x 3)))")
      deriv ("(* (* x y) (- x 3))", "x") should equal ("(+ (* x y) (* y (- x 3)))")
      deriv ("(* x y (- x 3))", "x") should equal ("(+ (* x y) (* y (- x 3)))")
      
      deriv ("(** x 1)", "x") should equal ("1")
      deriv ("(** x 2)", "x") should equal ("(* 2 x)")
      deriv ("(** x 2 3)", "x") should equal ("(* 6 (** x 5))")
      deriv ("(** x 3)", "x") should equal ("(* 3 (** x 2))")
      deriv ("(** x 4)", "x") should equal ("(* 4 (** x 3))")
      deriv ("(** x n)", "x") should equal ("(* n (** x (- n 1)))")
    }
  }
}
derivSpec execute