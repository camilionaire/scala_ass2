//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Testing ImpLang interpreter
//
import org.scalatest.FunSuite
import ILInterp._

class NullStream extends java.io.OutputStream { def write(x:Int) = {} }

class TestILI extends FunSuite {
  // supress console output
  def runSilent(s:String) = Console.withOut(new NullStream)(ILInterp(s))

  // compare expected and actual console output (could be multi-lines)
  def assertConsoleOutput(p:String, s:String) = {
    val output = new java.io.ByteArrayOutputStream(10240)
    def assertRes = assertResult(p){ILInterp(s);output.flush();output.toString}
    Console.withOut(output)(assertRes)
  }

  test("int literals and variable implicit init") {
    assertResult(0){ILInterp("0")}
    assertResult(-1){ILInterp("-1")}
    assertResult(12){ILInterp("12")}
    assertResult(0){ILInterp("x")}
  }
  
  test("arithmetic") {
    assertResult(3){ILInterp("(+ 1 2)")}
    assertResult(-3){ILInterp("(- 0 3)")}
    assertResult(12){ILInterp("(* 6 2)")}
    assertResult(12){ILInterp("(/ 24 2)")}
    intercept[Exception]{ILInterp("(/ 7 0)")}
  }

  test("comparison") {
    assertResult(1){ILInterp("(== 0 0)")}
    assertResult(0){ILInterp("(== 0 2)")}
  }

  test("if result") {
    assertResult(0){ILInterp("(if 1 0 1)")}
    assertResult(0){ILInterp("(if 0 1 0)")}
    assertResult(0){ILInterp("(if 2 0 1)")}
  }
  
  test("if evaluates only one branch") {
    assertConsoleOutput("0\n", "(if 0 (write 1) (write 0))")
    assertConsoleOutput("0\n", "(if 1 (write 0) (write 1))")
  }
  
  test("simple assignments") {
    assertResult(0){ILInterp("(:= s s)")}
    assertResult(1){ILInterp("(:= s 1)")}
    assertResult(1){ILInterp("(:= s (+ s 1))")}
  }
  
  test("write") {
    assertConsoleOutput("3\n", "(write 3)")
    assertConsoleOutput("2\n1\n", "(seq (write 2) (write 1))")
  }

  test("while returns 0") {
    assertResult(0){ILInterp("(while 0 0)")}
    assertResult(0){ILInterp("(seq (:= s 3) (while s (:= s (- s 1))))")}
  }
  
  test("while + assignment + write") {
    assertConsoleOutput("3\n2\n1\n",
      "(seq (:= s 3) (while s (seq (write s) (:= s (- s 1)))))")
  }

}
