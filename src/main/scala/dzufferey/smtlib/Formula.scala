package dzufferey.smtlib

import dzufferey.utils.Logger
import dzufferey.utils.LogLevel._

sealed abstract class Formula {

  def toStringFull: String

  var tpe: Type = Wildcard

  def setType(t: Type): this.type = {
    tpe = t
    this
  }

  var attributes: Seq[Attribute] = Seq()

  def setAttributes(a: Seq[Attribute]): this.type = {
    attributes = a
    this
  }


  def alpha(map: Map[Variable, Variable]): Formula

  val freeVariables: Set[Variable]
  val boundVariables: Set[Variable]
}

case class Literal[T <: AnyVal](value: T) extends Formula {

  value match {
    case _: Boolean => tpe = Bool
    case _: scala.Int => tpe = Int
    case _: scala.Long => tpe = Int
    case _: scala.Short => tpe = Int
    case _: scala.Byte => tpe = Int
    case _: scala.Float => tpe = Real
    case _: scala.Double => tpe = Real
    case _ => ()
  }

  override def toString = value.toString
  def toStringFull = "(" + toString + ": " + tpe + ")"

  def alpha(map: Map[Variable, Variable]) = this
  lazy val freeVariables = Set[Variable]()
  lazy val boundVariables = Set[Variable]()

}
object True {
  def unapply(f: Formula): Boolean = f match {
    case Literal(true) => true
    case _ => false
  }
  def apply(): Literal[Boolean] = Literal(true).setType(Bool)
}
object False {
  def unapply(f: Formula): Boolean = f match {
    case Literal(false) => true
    case _ => false
  }
  def apply(): Literal[Boolean] = Literal(false).setType(Bool)
}
object IntLit {
  def unapply(f: Formula): Option[scala.Int] = f match {
    case Literal(i: scala.Int) => Some(i)
    case _ => None
  }
  def apply(i: scala.Int): Literal[scala.Int] = Literal(i).setType(Int)
}

case class Variable(name: String) extends Formula {

  override def toString = name
  def toStringFull = "(" + toString + ": " + tpe + ")"

  def alpha(map: Map[Variable, Variable]) = map.getOrElse(this, this)
  lazy val freeVariables = Set[Variable](this)
  lazy val boundVariables = Set[Variable]()

}

case class Application(fct: Symbol, args: List[Formula]) extends Formula {

  fct.tpe match {
    case Function(_, ret) => tpe = ret
    case _ => ()
  }

  override def toString = fct.toString + args.mkString("(",", ",")")
  def toStringFull = "(" + fct.toString + args.map(_.toStringFull).mkString("(",", ",")") + ": " + tpe + ")"

  def alpha(map: Map[Variable, Variable]) = Application(fct, args.map(_.alpha(map)))
  lazy val freeVariables = args.foldLeft(Set[Variable]())(_ ++ _.freeVariables)
  lazy val boundVariables =  args.foldLeft(Set[Variable]())(_ ++ _.boundVariables)

}

object Fix extends Enumeration {
  type Fix = Value
  val Prefix, Infix, Suffix = Value
}

sealed abstract class Symbol {
  val typeParams: List[TypeVariable] = Nil
  val typeWithParams: Type

  def instanciateType(ts: List[Type]) = {
    val subst = typeParams.zip(ts).toMap
    typeWithParams alpha subst
  }

  def tpe: Type = instanciateType(typeParams.map( t => Type.freshTypeVar))
  
  def tpe(nbrArgs: Int): Type = { // linter:ignore UnusedParameter
    //this can be overridden variable arity symbols
    tpe
  }

  val fix = Fix.Prefix
  val priority = 10

  def apply(args: Formula*): Formula = {
    val t = tpe(args.length)
    if (args.lengthCompare(t.arity) != 0) {
      Logger("Formula", Warning, "arity of " + this + " is " + t.arity + ", given args: " + args.mkString(", "))
    }
    val app = Application(this, args.toList)
    val ret = Type.freshTypeVar
    //fill the type as much as possible
    val target = Function(args.map(_.tpe).toList, ret)
    Type.unify(t, target) match {
      case Some(subst) if subst contains ret =>
        //println(this + args.mkString("(",",",")") + ": " + subst)
        app.setType(subst(ret))
      case other =>
        //println(this + args.mkString("(",",",")") + " with "+t+" ⇔ "+target+": " + other)
        t match {
          case Function(_, TypeVariable(_)) => app
          case Function(_, ret) => app.setType(ret)
          case _ => app
        }
    }
  }

}

case class UnInterpretedFct(symbol: String,
                            _tpe: Option[Type] = None,
                            tParam: List[TypeVariable] = Nil) extends Symbol {
  override def toString = symbol
  
  def raw = symbol + tParam.mkString("[",",","]") + ": " + _tpe.map(_.toString).getOrElse("--")

  def stripType = UnInterpretedFct(symbol)

  override val typeParams = tParam
  val typeWithParams = _tpe.getOrElse(Wildcard)

  override val priority = 20

}

sealed abstract class InterpretedFct(val symbol: String, aliases: String*) extends Symbol {
  override def toString = symbol

  def allSymbols = symbol +: aliases

  def unapplySeq(f: Formula): Option[List[Formula]] = {
    val t = this
    f match {
      case Application(`t`, args) => Some(args)
      case _ => None
    }
  }
  override val fix = Fix.Infix
}

case object Not extends InterpretedFct("¬", "~", "!", "unary_!", "unary_$bang", "not") {
  val typeWithParams = Bool ~> Bool
  override val fix = Fix.Prefix
  override val priority = 8
}

case object And extends InterpretedFct("∧", "&&", "$amp$amp", "and", "And") {
  val typeWithParams = Bool ~> Bool ~> Bool
  override val priority = 5
  override def tpe(nbrArgs: Int): Type = Function((0 until nbrArgs).map( x => Bool).toList, Bool)
}
case object Or extends InterpretedFct("∨", "||", "$bar$bar", "or", "Or") {
  val typeWithParams = Bool ~> Bool ~> Bool
  override val priority = 4
  override def tpe(nbrArgs: Int): Type = Function((0 until nbrArgs).map( x => Bool).toList, Bool)
}
case object Implies extends InterpretedFct("⇒", "==>", "$eq$eq$greater", "=>", "Implies") {
  val typeWithParams = Bool ~> Bool ~> Bool
  override val priority = 3
}

case object Eq extends InterpretedFct("=", "==", "⇔", "$eq$eq") {
  private val fv = Type.freshTypeVar
  override val typeParams = List(fv)
  val typeWithParams = fv ~> fv ~> Bool
  override val priority = 9
}

case object Plus extends InterpretedFct("+", "$plus") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 10
  override def tpe(nbrArgs: Int): Type = Function((0 until nbrArgs).map( x => Int).toList, Int)
}
case object Minus extends InterpretedFct("-", "$minus") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 12
}
case object Times extends InterpretedFct("∙", "*", "$times") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 15
  override def tpe(nbrArgs: Int): Type = Function((0 until nbrArgs).map( x => Int).toList, Int)
}
case object Divides extends InterpretedFct("/", "$div") {
  val typeWithParams = Int ~> Int ~> Int
  override val priority = 15
}

case object Leq extends InterpretedFct("≤", "<=", "$less$eq") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}
case object Geq extends InterpretedFct("≥", ">=", "$greater$eq") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}
case object Lt extends InterpretedFct("<", "$less") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}
case object Gt extends InterpretedFct(">", "$greater") {
  val typeWithParams = Int ~> Int ~> Bool
  override val priority = 9
}

// Array functions
case object Store extends InterpretedFct("store", "$store") {
  private val i = Type.freshTypeVar
  private val v = Type.freshTypeVar
  override val typeParams = List(i,v)
  val typeWithParams = SArray(i,v) ~> i ~> v ~> SArray(i,v)
  override val priority = 20
}

case object Select extends InterpretedFct("select", "$select") {
  private val i = Type.freshTypeVar
  private val v = Type.freshTypeVar
  override val typeParams = List(i,v)
  val typeWithParams = SArray(i,v) ~> i ~> v
  override val priority = 20
}


object InterpretedFct {
  private var symbols: List[InterpretedFct] = Nil
  private var map: Map[String,InterpretedFct] = Map.empty
  def add(s: InterpretedFct): Unit = {
    symbols = s :: symbols
    map = s.allSymbols.foldLeft(map)( (m, sym) => {
      assert(!(map contains sym), "symbol redefinition: " + sym)
      m + (sym -> s)
    })
    assert(s.allSymbols.forall(map contains _))
  }
  def apply(s: String): Option[InterpretedFct] = {
    map.get(s)
  }
  def knows(s: String) = {
    val res = map contains s
    //println(s + " -> " + res)
    res
  }

  //need to be added manually since object are initialized lazily
  add( Not )
  add( And )
  add( Or )
  add( Implies )
  add( Eq )
  add( Plus )
  add( Minus )
  add( Times )
  add( Divides )
  add( Leq )
  add( Geq )
  add( Lt )
  add( Gt )
  add( Store )
  add( Select )
}


sealed abstract class BindingType

case class Binding(binding: BindingType, vs: List[Variable], f: Formula) extends Formula {

  override def toString = binding.toString + " " + vs.mkString(" ") + ". " + f
  def toStringFull = binding.toString + " " + vs.map(_.toStringFull).mkString(" ") + ". " + f.toStringFull

  def alpha(map: Map[Variable, Variable]) = Binding(binding, vs, f.alpha(map -- vs))
  lazy val freeVariables = f.freeVariables -- vs
  lazy val boundVariables = f.boundVariables ++ vs

}

case object ForAll extends BindingType {
  def unapply(f: Formula): Option[(List[Variable],Formula)] = f match {
    case Binding(ForAll, v, f) => Some(v,f)
    case _ => None
  }
  def apply(vs:List[Variable], f: Formula) = f match {
    case ForAll(vs2, f2) => Binding(ForAll, vs ::: vs2, f2)
    case _ => if (vs.isEmpty) f else Binding(ForAll, vs, f).setType(Bool)
  }
}
case object Exists extends BindingType {
  def unapply(f: Formula): Option[(List[Variable],Formula)] = f match {
    case Binding(Exists, v, f) => Some(v,f)
    case _ => None
  }
  def apply(vs:List[Variable], f: Formula) = f match {
    case Exists(vs2, f2) => Binding(Exists, vs ::: vs2, f2)
    case _ => if (vs.isEmpty) f else Binding(Exists, vs, f).setType(Bool)
  }
}


