//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22)
//-------------------------------------------------------------------------

// ImpLang - A simple imparative language
//
// Grammar:
//   Expr -> <num>                     // int const               
//        |  <id>                      // var                     
//        |  (Op Expr Expr)            // binop                   
//        |  (if Expr Expr Expr)       // if                      
//        |  (:= <id> Expr)            // assign                  
//        |  (write Expr)              // output                  
//        |  (seq Expr Expr)           // sequence of two exprs   
//        |  (while Expr Expr)         // while loop              
//        |  (for <id> Expr Expr Expr) // for loop                
//   Op   -> + | - | * | / | %         // arith op                
//        |  < | > | ==                // relational op           
//   <num>: integer  // "wholeNumber"                  
//   <id>:  string   // "ident"
//
import scala.util.parsing.combinator._

object ImpLang {
  sealed abstract class Expr
  case class Num(i:Int) extends Expr
  case class Var(id:String) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Div(l:Expr,r:Expr) extends Expr
  case class Rem(l:Expr,r:Expr) extends Expr
  case class Lt(l:Expr,r:Expr) extends Expr
  case class Gt(l:Expr,r:Expr) extends Expr
  case class Eq(l:Expr,r:Expr) extends Expr
  case class If(c:Expr,t:Expr,e:Expr) extends Expr
  case class Assgn(x:String,e:Expr) extends Expr
  case class Write(e:Expr) extends Expr
  case class Seq(a:Expr,b:Expr) extends Expr
  case class While(c:Expr,e:Expr) extends Expr
  case class For(x:String,a:Expr,b:Expr,e:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  object ILParse extends JavaTokenParsers {
    // input program is in s-expression form
    def expr: Parser[Expr] = atm | lst
    def atm: Parser[Expr] = 
          wholeNumber ^^ {s => Num(s.toInt)} |
          ident       ^^ {s => Var(s)}
    def lst: Parser[Expr] = 
          "(+" ~> expr~expr <~ ")"     ^^ {case l~r => Add(l,r)}   |
          "(-" ~> expr~expr <~ ")"     ^^ {case l~r => Sub(l,r)}   |
          "(*" ~> expr~expr <~ ")"     ^^ {case l~r => Mul(l,r)}   |
          "(/" ~> expr~expr <~ ")"     ^^ {case l~r => Div(l,r)}   |
          "(%" ~> expr~expr <~ ")"     ^^ {case l~r => Rem(l,r)}   |
          "(<" ~> expr~expr <~ ")"     ^^ {case l~r => Lt(l,r)}    |
          "(>" ~> expr~expr <~ ")"     ^^ {case l~r => Gt(l,r)}    |
          "(==" ~> expr~expr <~ ")"    ^^ {case l~r => Eq(l,r)}    | 
          "(if" ~> expr~expr~expr <~ ")" ^^ {case c~l~r => If(c,l,r)} |
          "(:=" ~> ident~expr <~ ")"   ^^ {case x~e => Assgn(x,e)} |
          "(write" ~> expr <~ ")"      ^^ {e => Write(e)} |
          "(seq" ~> expr~expr <~ ")"   ^^ {case a~b => Seq(a,b)}   |
          "(while" ~> expr~expr <~ ")" ^^ {case c~e => While(c,e)} | 
          "(for" ~> ident~expr~expr~expr <~ ")" ^^ {case x~a~b~e => For(x,a,b,e)} 

    def apply(text: String): Expr = {
      val result = parseAll(expr, text)
      result match {
        case Success(r, _) => r
        case Failure(msg, n) =>
          throw ParseException(msg +
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
        case Error(msg, n) =>
          throw ParseException(msg +
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
      }
    }
  }

  // Test driver
  // Test driver (two options)
  // 1. Input from StdIn:  linux> scala ImpLang -
  //                       ...program (in single line)...
  // 2. Input from file:   linux> scala ImpLang <filename>
  //
  import scala.io._
  def main(argv: Array[String]) = {
    try {
      val s = if (argv(0) == "-") StdIn.readLine()
              else Source.fromFile(argv(0)).getLines.mkString("\n")
      val p = ILParse(s)
      println("Input: " + s)
      println("AST:   " + p)
    } catch {
      case ex: ParseException => { println("Parser Error:" + ex.string) }
    }
  }
}    
//
