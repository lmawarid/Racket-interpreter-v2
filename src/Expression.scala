import scala.math

object Expression {
  private val BINARY_FUNCTION = 
    Array("+", "-", "/", "-", "quotient", "remainder", "modulo", "expt",
          "max", "min")
  private val UNARY_FUNCTION =
    Array("-", "/", "abs", "exp", "sqr", "sqrt", "sin", "cos", "tan")
    
  def isBinary(op : String) = Expression.BINARY_FUNCTION.contains(op)
  def isUnary(op : String) = Expression.UNARY_FUNCTION.contains(op)
}

abstract class Expression {
  def print() : String
  def eval(dict : Dictionary) : Double
}

class Number(val value : Double) extends Expression {
  def print() = value.toString()
  def eval(dict : Dictionary) = value
}

class Variable(val name : String)
  extends Expression {
  
  def print() = name
  def eval(dict : Dictionary) = dict.find(name)
}

class UnaryOperator(val op : String, var expr : Expression)
  extends Expression {
  
  def print() = "Unary: " + op + "  " + expr.print
  
  def eval(dict : Dictionary) : Double = op match {
    case "-" => -expr.eval(dict)
    case "/" => 1.0 / expr.eval(dict)
    case "abs" => math.abs(expr.eval(dict))
    case "exp" => math.exp(expr.eval(dict))
    case "sqr" => math.pow(expr.eval(dict), 2)
    case "sqrt" => math.sqrt(expr.eval(dict))
    case "sin" => math.sin(expr.eval(dict))
    case "cos" => math.cos(expr.eval(dict))
    case "tan" => math.tan(expr.eval(dict))
  }
}

class BinaryOperator(val op : String, var left : Expression,
    var right : Expression) extends Expression {
  
  def print() = "Binary: " + left.print + " " + op + " " + right.print
  
  def eval(dict : Dictionary) : Double = op match {
    case "+" => left.eval(dict) + right.eval(dict)
    case "-" => left.eval(dict) - right.eval(dict)
    case "*" => left.eval(dict) * right.eval(dict)
    case "/" => left.eval(dict) / right.eval(dict)
    case "quotient" => math.floor(left.eval(dict) / right.eval(dict))
    case "remainder" => left.eval(dict) % right.eval(dict)
    case "modulo" => left.eval(dict) % right.eval(dict)
    case "expt" => math.pow(left.eval(dict), right.eval(dict))
    case "max" => math.max(left.eval(dict), right.eval(dict))
    case "min" => math.min(left.eval(dict), right.eval(dict))
  }
}