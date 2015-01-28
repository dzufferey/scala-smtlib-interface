package dzufferey.smtlib

import org.scalatest._

class FormulaSuite extends FunSuite {

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

}
