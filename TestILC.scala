//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Testing ImpLang interpreter
//
import org.scalatest.FunSuite
import ILComp._
import RegIR._

class NullStream extends java.io.OutputStream { def write(x:Int) = {} }

class TestILC extends FunSuite {
  // supress console output
  def runSilent(s:String) = Console.withOut(new NullStream)(ILComp(s))

  // compare expected and actual console output (could be multi-lines)
  def assertConsoleOutput(p:String, s:String) = {
    val output = new java.io.ByteArrayOutputStream(10240)
    def assertRes = assertResult(p){ILComp(s);output.flush();output.toString}
    Console.withOut(output)(assertRes)
  }

  test("int literals and variable implicit init") {
    assertResult(0){ILComp("0")}
    assertResult(-1){ILComp("-1")}
    assertResult(12){ILComp("12")}
    assertResult(0){ILComp("x")}
  }
  
  test("arithmetic") {
    assertResult(3){ILComp("(+ 1 2)")}
    assertResult(-3){ILComp("(- 0 3)")}
    assertResult(12){ILComp("(* 6 2)")}
    assertResult(12){ILComp("(/ 24 2)")}
    intercept[Exception]{ILComp("(/ 7 0)")}
  }

  test("comparison") {
    assertResult(1){ILComp("(== 0 0)")}
    assertResult(0){ILComp("(== 0 2)")}
  }

  test("if result") {
    assertResult(0){ILComp("(if 1 0 1)")}
    assertResult(0){ILComp("(if 0 1 0)")}
    assertResult(0){ILComp("(if 2 0 1)")}
  }
  
  test("if evaluates only one branch") {
    assertConsoleOutput("0\n", "(if 0 (write 1) (write 0))")
    assertConsoleOutput("0\n", "(if 1 (write 0) (write 1))")
  }
  
  test("simple assignments") {
    assertResult(0){ILComp("(:= s s)")}
    assertResult(1){ILComp("(:= s 1)")}
    assertResult(1){ILComp("(:= s (+ s 1))")}
  }
  
  test("write") {
    assertConsoleOutput("3\n", "(write 3)")
    assertConsoleOutput("2\n1\n", "(seq (write 2) (write 1))")
  }

  test("while returns 0") {
    assertResult(0){ILComp("(while 0 0)")}
    assertResult(0){ILComp("(seq (:= s 3) (while s (:= s (- s 1))))")}
  }
  
  test("while + assignment + write") {
    assertConsoleOutput("3\n2\n1\n",
      "(seq (:= s 3) (while s (seq (write s) (:= s (- s 1)))))")
  }

}
