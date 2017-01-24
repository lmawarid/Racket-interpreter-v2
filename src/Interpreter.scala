import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
import java.util.Scanner

object Interpreter {
  def isBlank(str : String) : Boolean = {
    val len = str.length()
    var index = 0
    
    while (index < len && str(index) == ' ') index += 1
    
    index == len
  }
  
  def parse(expr : String, parens : Array[Int]) : Array[String] = {
    var tokens = new ArrayBuffer[String]()
    val len = expr.length()
    var pos = 0
    
    var token = new StringBuilder()
    
    while (pos < len) {
      var char = expr(pos)
      
      if (char.isWhitespace) {
        pos += 1
      } else if (char == '(' || char == ')') {
          if (char == '(') parens(0) += 1
          else parens(1) += 1
          
          val paren = char.toString()
          tokens += paren
          pos += 1
      } else if (char.isDigit) {
          while ((char.isDigit || char == '.') && pos < len) {
            token += char
            pos += 1
          
            if (pos < len) char = expr(pos) 
          }
          
          tokens += token.toString()
          token.setLength(0)
      } else {
          while (!(char.isWhitespace || char == ')') &&
                 pos < len) {
            token += char
            pos += 1
            
            if (pos < len) char = expr(pos)
          }
          
          tokens += token.toString()
          token.setLength(0)
      }
    }
    
    tokens.toArray
  }
  
  def display(tokens : Array[String]) : Unit = {
    print("[ ")
    for (str <- tokens) print(str + " ")
    println("] ")
  }
  
  def main(args : Array[String]) {
    var dict = new Dictionary()
    val in = new Scanner(System.in)
    var parsedStream = new ArrayBuffer[String]()
    var parens = Array(0, 0)
    
    do {
      var expr = in.nextLine()
      
      if (!isBlank(expr)) {
        var tokens = parse(expr, parens)
        parsedStream ++= tokens
        //display(parsedStream.toArray)
        
        if (parens(0) == parens(1)) {
          var rexp = new RacketExpr(parsedStream.toArray, dict)
          
          //println("Entered " + rexp.print)
          if (!parsedStream.contains("define")) println(rexp.interpNum)
            
          parens(0) = 0
          parens(1) = 0
          rexp.reset
          parsedStream.clear()
        }
      }
    } while (in.hasNextLine())
      
    in.close()
  }
}