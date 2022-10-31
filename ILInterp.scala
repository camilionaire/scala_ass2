// ImpLang Interpreter
//
// Usage: linux> scala ILInterp <source file>
//
// Name: Camilo Schaser-Hughes
// Date: October 31, 2022
// Class: CS558 Prog. Lang.
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
        // store.getOrElse returns x or 0.
        case Var(x) => store.getOrElse(x, 0) 
        case Add(l,r) => interpE(l) + interpE(r)
        case Sub(l,r) => interpE(l) - interpE(r)
        case Mul(l,r) => interpE(l) * interpE(r)
        case Div(l,r) => {
          // interps left and right and then checks for zero.
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl / vr
        }
        case Rem(l,r) => { 
          // same as div
          val vl = interpE(l)
          val vr = interpE(r)
          if (vr == 0)
            throw InterpException("divide by zero")
          else
            vl % vr
          } 
        // interps and compares, returns 1 or 0
        case Lt(l,r)  => if (interpE(l) < interpE(r)) 1 else 0
        case Gt(l,r)  => if (interpE(l) > interpE(r)) 1 else 0
        case Eq(l,r)  => if (interpE(l) == interpE(r)) 1 else 0 
        // I think it's fine to do the branching like this?
        case If(c,t,e)  => if (interpE(c) > 0) interpE(t) else interpE(e)
        case Assgn(x,e) => {
          val ve = interpE(e)
          // was the only way of storing I could get working for some reason.
          store(x) = ve
          ve
        }
        // just writes ve and then returns the value
        case Write(e)   => {
          val ve = interpE(e)
          print(ve + "\n")
          ve
        }
        // does one and then the other, returning result of second.
        case Seq(e1,e2) => {
          interpE(e1)
          interpE(e2)
        }
        // checks if condition is met, 
        // does a recursive call... not sure if should
        // instead be a not equal to instead.
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
          // does a substitution into the whilel loop
          // the condition is greater than -1 for the difference btwn two nums
          // instead of greater/equal to 0... 
          interpE(While(Gt(Sub(e2, Var(x)), Num(-1)),
          // sequence does the operation of while/for loop then increments
           Seq(e3, Assgn(x, Add(Var(x), Num(1))))))
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
