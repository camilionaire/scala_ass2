//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/2022)
//-------------------------------------------------------------------------

// Register Machine IR
//

// Program -> {Inst} EndInst
// 
// Inst    -> <id> = Src AOP Src            // Binop          
//         |  <id> = Src                    // Move     
//         |  if Src ROP Src goto <lab>     // CJump    
//         |  goto <lab>                    // Jump     
//         |  <lab> :                       // Label decl 
//         |  print Src                     // Print
// EnInst  -> return Src                    // Return prog's value 
// Src     -> <num> | <id> 
// AOP     -> + | - | * | / | %
// ROP     -> < | > | ==
//
// <num>: integer   // integer literal
// <id>:  string    // var or temp name
// <lab>: integer   // label num
//

object RegIR {

  object AOP extends Enumeration {
    val Add, Sub, Mul, Div, Rem = Value
  }
  object ROP extends Enumeration {
    val Lt, Gt, Eq = Value
  }

  sealed abstract class Src
  case class Name(s:String) extends Src
  case class Const(i:Int) extends Src

  sealed abstract class Inst
  case class Bop(op:AOP.Value,d:String,x:Src,y:Src) extends Inst
  case class Mov(d:String,x:Src) extends Inst
  case class Print(x:Src) extends Inst
  case class CJump(op:ROP.Value,x:Src,y:Src,l:Int) extends Inst
  case class Jump(l:Int) extends Inst
  case class Label(l:Int) extends Inst
  case class Return(x:Src) extends Inst

  case class ExecException(string: String) extends RuntimeException

  type Program = List[Inst]

  def exec(prog:Program, debug:Int = 0): Int = {
    val store = collection.mutable.Map[String,Int]()
    var pc = 0

    def getSrcVal(x:Src): Int = x match {
      case Name(s) => store.getOrElse(s,0)
      case Const(i)  => i
    }

    def findLabel(prog:Program,l:Int) = {
      def f(n:Int,prog:Program): Int = prog match {
        case Nil => throw ExecException("missing label " + l)
        case Label(l1)::rest if l == l1 => n
        case _::rest => f(n+1,rest)
      }
      f(0,prog)
    }

    def step(): Int = prog(pc) match {
      case Bop(op,d,x,y) => {
        val xv = getSrcVal(x)
        val yv = getSrcVal(y)
        op match {
          case AOP.Add => store.update(d, xv + yv)
          case AOP.Sub => store(d) = xv - yv
          case AOP.Mul => store(d) = xv * yv
          case AOP.Div => {
            if (yv == 0)
              throw ExecException("division by zero")
            store(d) = xv / yv
          }
          case AOP.Rem => {
            if (yv == 0)
              throw ExecException("division by zero")
            store(d) = xv % yv
          }
        }
        pc+1
      }
      case Mov(d,x) => {
        val v = getSrcVal(x)
        store(d) = v
        pc+1
      }
      case Print(x) => {
        val v = getSrcVal(x)
      	printf(v+"\n");
        pc+1
      }
      case CJump(op,x,y,l) => {
        val xv = getSrcVal(x)
        val yv = getSrcVal(y)
        val cond = op match {
          case ROP.Eq => xv == yv
          case ROP.Lt => xv < yv
          case ROP.Gt => xv > yv
        }
        if (cond)
          findLabel(prog,l)
        else
          pc+1
      }
      case Jump(l)  => findLabel(prog,l)
      case Label(l) => pc+1

      case _ => throw ExecException("unexpected inst")
    }

    val plen = prog.length - 1
    while (pc < plen) {
      if (debug > 1) print("" + pc + ": " + prog(pc))
      pc = step()
    }
    prog(plen) match {
      case Return(x) => return getSrcVal(x)
      case _ => throw ExecException("missing end inst")
    }
  }
}
