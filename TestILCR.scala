//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Testing ImpLang to RegM1 compiler
//
import org.scalatest.FunSuite
import ILCompReg._

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

  // Programs used in the tests

  val easyProgram1 = "(for i 0 10 (write i))"
  val easyProgram2 = "(for i 0 9 (for j 0 9 (write (+ j (* 10 i)))))"

  // this should print 2 8 and evaluate to 0
  val trickyProgram1 = 
    "(for i 0 (seq (:= i (+ i 2)) 10) (seq (write i) (:= i (+ i 3))))"

  val trickyProgram2 = 
    "(seq (for i 3 (seq (:= i (+ s 8)) 44) (seq (write i) (:= s (+ i 7)))) i)"

  // store side effects of for loop modifying variables from outside
  val sumProgram = "(seq (for i 0 10 (seq (write s) (:= s (+ s i)))) s)"


  // find all primes between 2 to 50
  // n = 2;
  // do {
  //   prime = 1;    
  //   d = 2;
  //   do {
  //     if (n % d == 0)
  //       prime = 0;
  //     d = d + 1;
  //   } until (n == d);
  //   if (prime) 
  //     write n;
  //   n = n + 1;
  // } until (n = 51);
  //      
  val primesProgram = 
    """(seq (seq
         (write 2)
         (:= n 3))
         (do (seq (seq (seq (seq
               (:= prime 1)
               (:= d 2))
               (do (seq
                     (if (% n d) 0 (:= prime 0))
                     (:= d (+ d 1)))
                   (== d n)))
               (if prime (write n) 0))
               (:= n (+ n 1)))
             (== n 50)))"""

  // n = 2;
  // while (n < 51) {
  //   prime = 1;    
  //   d = 2;
  //   while (n > d) {
  //     if (n % d == 0)
  //       prime = 0;
  //     d = d + 1;
  //   }
  //   if (prime) 
  //     write n;
  //   n = n + 1;
  // }
  val primesProgram2 = 
    """(seq
         (:= n 2)
         (while (< n 51)
           (seq (seq (seq (seq
             (:= prime 1)
             (:= d 2))
             (while (< d n)
               (seq
                 (if (% n d) 0 (:= prime 0))
                 (:= d (+ d 1)))))
             (if prime (write n) 0))
             (:= n (+ n 1)))))"""

  val primesResult = 
    "2\n3\n5\n7\n11\n13\n17\n19\n23\n29\n31\n37\n41\n43\n47\n"

  test("variable implicit init") {
    assertResult(0){ILComp("x")}
    assertResult(0){ILComp("foo")}
    assertResult(0){ILComp("bar1")}
  }

  test("int literals") {
    assertResult(0){ILComp("0")}
    assertResult(-1){ILComp("-1")}
    assertResult(12){ILComp("12")}
  }
  
  test("arithmetic") {
    assertResult(0){ILComp("(* 0 7)")}
    assertResult(-3){ILComp("(- 0 3)")}
    assertResult(12){ILComp("(* 6 2)")}
    assertResult(12){ILComp("(/ 24 2)")}
    intercept[Exception]{ILComp("(/ 7 0)")}
  }

  test("comparison") {
    assertResult(1){ILComp("(== 0 0)")}
    assertResult(0){ILComp("(== 0 2)")}
  }

  test("simple assignments") {
    assertResult(0){ILComp("(:= s s)")}
    assertResult(1){ILComp("(:= s 1)")}
  }
  
  test("while returns 0") {
    assertResult(0){ILComp("(while 0 0)")}
    assertResult(0){ILComp("(seq (:= s 3) (while s (:= s (- s 1))))")}
  }
  
  test("if result") {
    assertResult(0){ILComp("(if 1 0 1)")}
    assertResult(0){ILComp("(if 0 1 0)")}
    assertResult(0){ILComp("(if 2 0 1)")}
  }
  
  test("write") {
    assertConsoleOutput("3\n", "(write 3)")
    assertConsoleOutput("2\n1\n", "(seq (write 2) (write 1))")
  }

  test("if evaluates only one branch") {
    assertConsoleOutput("0\n", "(if 0 (write 1) (write 0))")
    assertConsoleOutput("0\n", "(if 1 (write 0) (write 1))")
  }
  
  test("while + assignment + write") {
    assertConsoleOutput("3\n2\n1\n",
      "(seq (:= s 3) (while s (seq (write s) (:= s (- s 1)))))")
  }

  test("Simple for loops, each returns a 0") {
    assertResult(0){runSilent(easyProgram1)}
    assertResult(0){runSilent(easyProgram2)}
  }

  test("Tricky for loop1") {
    assertResult(0){runSilent(trickyProgram1)}
  }
  
  test("Tricky for loop2") {
    assertResult(53){runSilent(trickyProgram2)}
  }
  
  test("Sum of 1 to 10") {
    assertResult(55){runSilent(sumProgram)}
  }

  test("For loop with side effects") {
    assertConsoleOutput("3\n1\n3\n2\n3\n3\n3\n",
      "(for i 1 (write 3) (write i))")
  }

  test("Primes that are < 50") {
//    assertConsoleOutput(primesResult, primesProgram) 
    assertConsoleOutput(primesResult, primesProgram2) 
  }

}
