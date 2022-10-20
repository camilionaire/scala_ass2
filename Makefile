# Makefile for ImpLang
#

.SUFFIXES: .scala .class

.PRECIOUS: %.scala

.scala.class:
	scalac $*.scala

ILInterp.class: ImpLang.class
ILComp.class: ImpLang.class RegIR.class
TestILI.class: ILInterp.class 
TestILC.class: ILComp.class 
TestILI2.class: ILInterp.class 
TestILC2.class: ILComp.class 

il:	 ImpLang.class
ili: ILInterp.class
reg: RegIR.class
ilc: ILComp.class
testili:  TestILI.class
testilc:  TestILC.class
testili2: TestILI2.class
testilc2: TestILC2.class
all: testili testili2 testilc testilc2 

clean:	
	rm *.class
