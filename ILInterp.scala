// ImpLang Interpreter
//
// Usage: linux> scala ILInterp <source file>
//
import ImpLang._

object ILInterp {
  case class InterpException(string: String) extends RuntimeException

  def interp(e:Expr, debug:Int = 0): Int = {
    val store = collection.mutable.Map[String,Int]()

    def interpE(e:Expr): Int = {
      if (debug > 1) {
        println("  expr = " + e);
        println("  store = " + store)
      }
      e match {
        case Num(n) => n
        case Var(x) => // ... need code ...   
        case Add(l,r) => interpE(l) + interpE(r)
        case Sub(l,r) => interpE(l) - interpE(r)
        case Mul(l,r) => interpE(l) * interpE(r)
        case Div(l,r) => {
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl / vr
        }
        case Rem(l,r) => // ... need code ...   
        case Lt(l,r)  => // ... need code ...  
        case Gt(l,r)  => // ... need code ...  
        case Eq(l,r)  => // ... need code ...  
        case If(c,t,e)  => // ... need code ... 
        case Assgn(x,e) => // ... need code ... 
        case Write(e)   => // ... need code ... 
        case Seq(e1,e2) => // ... need code ...
        case While(c,b) => // ... need code ...
        case For(x,e1,e2,e3) => // ... need code ...

      }
    }

    val v = interpE(e)
    if (debug > 0) println("Evaluates to: " + v)
    v
  } 
  
  def apply(s:String, debug:Int = 0): Int = {
    if (debug > 0) println("Input:  " + s)
    val e = ILParse(s)
    if (debug > 0) println("AST:    " + e)
    interp(e,debug)
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    try {
      val s = Source.fromFile(argv(0)).getLines.mkString("\n")
      val d = if (argv.length > 1) argv(1).toInt else 0
      val r = apply(s,d)
      println(r)
    } catch {
      case ex: ParseException =>  println("Parser Error: " + ex.string)
      case ex: InterpException => println("Interp Error: " + ex.string)
    }
  }
}
//
