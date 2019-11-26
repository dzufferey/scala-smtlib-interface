package dzufferey.smtlib

import scala.language.implicitConversions

object InlineOps {

  implicit def intToLit(i: Int): Formula = IntLit(i)

  implicit class Ops(lhs: Formula) {
  
    def unary_! = Not(lhs)
    def &&(rhs: Formula) = And(lhs, rhs)
    def ∧(rhs: Formula) = And(lhs, rhs)
    def ||(rhs: Formula) = Or(lhs, rhs)
    def ∨(rhs: Formula) = Or(lhs, rhs)

    def ==>(rhs: Formula) = Implies(lhs, rhs)
    def ===(rhs: Formula) = Eq(lhs, rhs)
    def !==(rhs: Formula) = Not(Eq(lhs, rhs))
    def ≠(rhs: Formula)   = Not(Eq(lhs, rhs))

    def +(rhs: Formula) = Plus(lhs, rhs)
    def -(rhs: Formula) = Minus(lhs, rhs)
    def *(rhs: Formula) = Times(lhs, rhs)
    def /(rhs: Formula) = Divides(lhs, rhs)
    
    def <=(rhs: Formula) = Leq(lhs, rhs)
    def >=(rhs: Formula) = Geq(lhs, rhs)
    def ≤(rhs: Formula) = Leq(lhs, rhs)
    def ≥(rhs: Formula) = Geq(lhs, rhs)
    def <(rhs: Formula) = Lt(lhs, rhs)
    def >(rhs: Formula) = Gt(lhs, rhs)

  }

}
