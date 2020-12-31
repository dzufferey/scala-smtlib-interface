package dzufferey.smtlib

import org.scalatest.funsuite.AnyFunSuite

class ParserSuite extends AnyFunSuite {

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

  test("parsing with comments"){
    val str = """
(forall ((r $Ref)) (;! r@222
  true
  ; :pattern ((down r))
  ; :qid |name|
))
"""
    val parsed = Parser.parseTerm(str)
    val expected = ForAll(List(Variable("r")), True())
    assert(parsed == Some(expected))
  }

  test("parsing with annotations 1"){
    val str = "(forall ((r $Ref)) (! r@222 :pattern ((down r)) :qid |name|))"
    val parsed = Parser.parseTerm(str)
    val expected = ForAll(List(Variable("r")), Variable("r@222"))
    assert(parsed == Some(expected))
    val printed = Printer.toString(parsed.get)
    val expected1 = "(forall ((r $Ref)) (! r@222 :pattern ((down r) ) :qid |name|))"
    assert(printed == expected1)
  }

  test("parsing with annotations 2"){
    val str = """
(forall ((r $Ref)) (!
  true
  :pattern ((down r))
))
"""
    val parsed = Parser.parseTerm(str)
    val expected = ForAll(List(Variable("r")), True())
    assert(parsed == Some(expected))
    parsed match {
      case Some(ForAll(_, t @ True())) =>
        assert(t.attributes.length == 1)
        val pat = Application(UnInterpretedFct("down"), List(Variable("r")))
        assert(t.attributes.head == AttrExpr(":pattern", List(pat)))
      case _ => assert(false)
    }
    val printed = Printer.toString(parsed.get)
    val expected1 = "(forall ((r $Ref)) (! true :pattern ((down r) )))"
    assert(printed == expected1)
  }

}
