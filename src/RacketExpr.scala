object RacketExpr {
  
  private var index = 0
  private def update(newIndex : Int) = { this.index = newIndex }
}

class RacketExpr(var tokens : Array[String], var dict : Dictionary) {
  
  def parse() : Expression = {
    val len = tokens.length
    var pos = RacketExpr.index
    var expr = None : Option[Expression]
    
    if (pos < len) {
      var token = tokens(pos)
      
      if (token == "(") {
        pos += 1
        
        if (tokens(pos) == "define") {
          val name = tokens(pos + 1)
          RacketExpr.update(pos + 2)
          val value = parse().eval(dict)
          
          dict.add(name, value)
          expr = Some(new Variable(name))
        } else if (Expression.isBinary(tokens(pos))) {
          val operator = tokens(pos)
          pos += 1
          RacketExpr.update(pos)
          
          val left = parse()
          val right = parse()
          
          pos = RacketExpr.index
          if (right == null) {
            expr = Some(new UnaryOperator(operator, left))
          } else {
            expr = Some(new BinaryOperator(operator, left, right))
          }
        } else if (Expression.isUnary(tokens(pos))) {
          val operator = tokens(pos)
          pos += 1
          RacketExpr.update(pos)
          
          val operand = parse()
          pos = RacketExpr.index
          
          expr = Some(new UnaryOperator(operator, operand))
        } else {
          expr = None
        }
      } else if (token == ")") {
          pos += 1
          RacketExpr.update(pos)
          expr = Some(parse())
      } else if (tokens(pos)(0).isLetter) {
          token = tokens(pos)
          expr = Some(new Variable(token))
      } else if ((tokens(pos)(0) == '-' && tokens(pos)(1).isDigit) ||
                (tokens(pos)(0).isDigit)) {
          token = tokens(pos)
          expr = Some(new Number(token.toDouble))
      } else {
        expr = None
      }
      
      pos += 1
      RacketExpr.update(pos)
    }
    
    expr match {
      case None => null
      case Some(expression) => expression
    }
  }
  
  private var expr = parse()
  
  def print = expr.print
  def interpNum = expr.eval(dict)
  def reset = RacketExpr.update(0)
}