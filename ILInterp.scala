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
        case Var(x) => store.getOrElse(x, 0) // DONE????
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
        case Rem(l,r) => { 
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl % vr
          } // done?
        case Lt(l,r)  => {
          if (interpE(l) < interpE(r)) 1 else 0
          } // done?
        case Gt(l,r)  => if (interpE(l) > interpE(r)) 1 else 0// done?
        case Eq(l,r)  => if (interpE(l) == interpE(r)) 1 else 0 // done?
        case If(c,t,e)  => {
          if (interpE(c) > 0) {
            interpE(t)
          } else {
            interpE(e)
          }
        } // done?
        case Assgn(x,e) => {
          val ve = interpE(e)
          store(x) = ve
          ve
        }
        case Write(e)   => {
          val ve = interpE(e)
          print(ve + "\n")
          ve
        }
        case Seq(e1,e2) => {
          interpE(e1)
          interpE(e2)
        }
        case While(c,b) => {
          if (interpE(c) > 0) {
            interpE(b)
            interpE(While(c,b))
          } else
            0
        }
        case For(x,e1,e2,e3) => {
          val v1 = interpE(e1)
          store(x) = v1
          val v2 = interpE(e2)
          val vx = store.getOrElse(x, 0)
          if (vx <= v2) {
            interpE(e3)
            interpE(For(x, Add(Num(1), e1), e2, e3))
          } else 0
        }
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
