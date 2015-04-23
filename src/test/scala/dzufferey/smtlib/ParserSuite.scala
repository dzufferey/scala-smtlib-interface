package dzufferey.smtlib

import org.scalatest._

class ParserSuite extends FunSuite {

  test("parsing term 0"){
    val model = Parser.parseTerm("(and (not (= x t)) (= y t))")
    assert(model == Some(And(Not(Eq(Variable("x"), Variable("t"))), Eq(Variable("y"), Variable("t")))))
  }

  test("parsing term 1"){
    val model = Parser.parseTerm("(and (= x (sin t)) (= y (cos t)))")
    assert(model == Some(And(Eq(Variable("x"), DRealDecl.sin(Variable("t"))), Eq(Variable("y"), DRealDecl.cos(Variable("t"))))))
  }

  test("parsing term 2"){
    val model = Parser.parseTerm("(> (+ (^ x 2) (^ y 2)) 1.2)")
    assert(model == Some(Gt(Plus(DRealDecl.pow(Variable("x"), Literal(2)), DRealDecl.pow(Variable("y"), Literal(2))), Literal(1.2))))
  }

}
