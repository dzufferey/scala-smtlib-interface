package dzufferey.smtlib

import org.scalatest.funsuite.AnyFunSuite

class FormulaSuite extends AnyFunSuite {

//import dzufferey.utils.Logger
//Logger.moreVerbose
//Logger.moreVerbose

  test("variable arity") {
    val a = Variable("a").setType(Int)
    val b = Variable("b").setType(Int)
    val c = Variable("c").setType(Int)
    val fa = Eq(a, Literal(0))
    val fb = Eq(Plus(a, b), Literal(1))
    val fc = Eq(Plus(a, b, c), Literal(2))
    val form = And(fa,fb,fc)
    val solver = Solver(QF_LIA)
    assert( solver.testB(form), "sat formula")
  }
  
  test("inline ops") {
    import InlineOps._
    val a = Variable("a").setType(Int)
    val b = Variable("b").setType(Int)
    val c = Variable("c").setType(Int)
    assert( (a + b === c) == Eq(Plus(a, b), c))
    assert( (a * c + b === 0) == Eq(Plus(Times(a, c), b), Literal(0)))
  }

}
