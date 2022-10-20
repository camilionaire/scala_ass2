//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// J. Li (8/22) [Based on A. Tolmach's code]
//-------------------------------------------------------------------------

// Testing ImpLang interpreter for the 'for' loop
//
import org.scalatest.FunSuite
import ILInterp._

class NullStream extends java.io.OutputStream { def write(x:Int) = {} }

class TestILI2 extends FunSuite {
  // supress console output
  def runSilent(s:String) = Console.withOut(new NullStream)(ILInterp(s))

  // compare expected and actual console output (could be multi-lines)
  def assertConsoleOutput(p:String, s:String) = {
    val output = new java.io.ByteArrayOutputStream(10240)
    def assertRes = assertResult(p){ILInterp(s);output.flush();output.toString}
    Console.withOut(output)(assertRes)
  }

  // Programs used in the tests

  // in addition to print outs, for loop should aways evaluate to 0
  val easyProgram1 = "(for i 0 10 (write i))"
  val easyProgram2 = "(for i 0 9 (for j 0 9 (write (+ j (* 10 i)))))"

  // for loop with side effects
  val easyProgram3 = "(for i 1 (write 3) (write i))"

  // store side effects of for loop modifying variables from outside
  val sumProgram = "(seq (for i 0 10 (seq (write s) (:= s (+ s i)))) s)"

  // this should print 2 8 and return 0
  val trickyProgram1 = 
    "(for i 0 (seq (:= i (+ i 2)) 10) (seq (write i) (:= i (+ i 3))))"

  // this should print 8 23 38 and return 53
  val trickyProgram2 = 
    "(seq (for i 3 (seq (:= i (+ s 8)) 44) (seq (write i) (:= s (+ i 7)))) i)"


  // find all primes between 2 to 50
  //   n = 2;
  //   while (n < 50) {
  //     prime = 1;    
  //     d = 2;
  //     while (n > d) {
  //       if (n % d == 0)
  //         prime = 0;
  //       d = d + 1;
  //     }
  //     if (prime) 
  //       write n;
  //     n = n + 1;
  //   } 
  val primesProgram = 
    """(seq
         (:= n 2)
         (while (< n 50)
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

  test("Simple for loops, each returns a 0") {
    assertResult(0){runSilent(easyProgram1)}
    assertResult(0){runSilent(easyProgram2)}
  }

  test("For loop with side effects") {
    assertConsoleOutput("3\n1\n3\n2\n3\n3\n3\n", easyProgram3)
  }

  test("Sum of 1 to 10") {
    assertResult(55){runSilent(sumProgram)}
  }

  test("Tricky for loop1") {
    assertResult(0){runSilent(trickyProgram1)}
  }
  
  test("Tricky for loop2") {
    assertResult(53){runSilent(trickyProgram2)}
  }
  
  test("Primes that are < 50") {
    assertConsoleOutput(primesResult, primesProgram)
  }

}
