package dzufferey.smtlib

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

object Names {
  
  def symbol(i: Symbol): String = i match {
    case Implies => "=>"
    case Or => "or"
    case And => "and"
    case Not => "not"
    case Eq => "="
    case Geq => ">="
    case Leq => "<="
    case Gt => ">"
    case Lt => "<"
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    //case Divides => "/" //TODO real division
    case Divides => "div" //this is for integers
    case Select => "select"
    case Store => "store"
    case UnInterpretedFct(f, _, _) => f
  }

  def overloadedSymbol(i: Symbol, ts: List[Type]) = i match {
    case Eq => "=" //already overloaded in the theory definition
    case normal => symbol(normal) + ts.map(tpe).mkString("","","")
  }

  def tpe(t: Type): String = t match {
    case Bool => "Bool"
    case Int => "Int"
    case Real => "Real"
    case SArray(t1, t2) => "(Array "+tpe(t1)+" "+tpe(t2)+")"
    case UnInterpreted(id) => id
    case Wildcard => Logger.logAndThrow("smtlib", Error, "Wildcard types should have been instanciated!")
    case other => Logger.logAndThrow("smtlib", Error, "not supported: " + other)
  }
  
  def tpeArity(t: Type): Int = t match {
    case SArray(_, _) => 2
    case _ => 0
  }
  
  def typeDecl(t: Type) = {
    val (args, ret) = t match {
      case Function(args, r) => (args, r)
      case other => (Nil, other)
    }
    val argsDecl = args.map(tpe).mkString("("," ",")")
    argsDecl + " " + tpe(ret)
  }
  
  val ite = {
    val fv = Type.freshTypeVar
    UnInterpretedFct("ite", Some(Bool ~> fv ~> fv ~> fv), List(fv))
  }

}
