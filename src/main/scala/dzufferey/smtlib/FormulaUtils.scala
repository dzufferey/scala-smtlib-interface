package dzufferey.smtlib

object FormulaUtils {

  class Traverser {
    def traverse(f: Formula): Unit = f match {
      case Literal(value) => ()
      case Variable(v) => ()
      case Application(fct, args) =>
        args foreach traverse
      case Binding(_, vs, f) =>
        vs foreach traverse
        traverse(f)
    }
  }

  abstract class Transformer {

    def transform(f: Formula): Formula = f match {
      case l @ Literal(_) => l
      case v @ Variable(_) => v
      case f @ Application(fct, args) =>
        val args2 = args map transform
        Application(fct, args2).setType(f.tpe)
      case b @ Binding(bt, vs, f) =>
        val vs2 = vs map transform map (_.asInstanceOf[Variable]) //this is bad but ...
        val f2 = transform(f)
        Binding(bt, vs2, f2).setType(Bool)
    }

  }

  class Mapper(fct: Formula => Formula) extends Transformer {
    override def transform(f: Formula): Formula = f match {
      case b @ Binding(bt, vs, f) =>
        //avoid capture
        def fct2(f: Formula): Formula = if (vs.exists(v => f == v)) f else fct(f)
        val m2 = new Mapper(fct2)
        fct(Binding(bt, vs, m2.transform(f)).setType(Bool))
      case other =>
        fct(super.transform(f))
    }
  }

  def map(fct: Formula => Formula, f: Formula): Formula = {
    val m = new Mapper(fct)
    m.transform(f)
  }
                  
  def replace(from: Formula, to: Formula, f: Formula): Formula = {
    def fct(e: Formula) = if (e == from) to else e
    val m = new Mapper(fct)
    m.transform(f)
  }


  def getConjuncts(f: Formula): List[Formula] = f match {
    case And(lst @ _*) => lst.flatMap(getConjuncts).toList
    case other => List(other)
  }
  
  def getDisjuncts(f: Formula): List[Formula] = f match {
    case Or(lst @ _*) => lst.flatMap(getDisjuncts).toList
    case other => List(other)
  }

  def typeParams(app: Application): List[Type] = app.fct match {
    case _: InterpretedFct => //skip those: defined/overloaded in smtlib
      Nil
    case normal =>
      val params = normal.typeParams
      val concreteType = Function(app.args.map(_.tpe), app.tpe)
      val subst = Type.unify(normal.typeWithParams, concreteType)
      subst match {
        case Some(s) => params.map(s)
        case None => sys.error("FormulaUtils.typeWithParams("+app.fct+"), cannot unify: " + normal.typeWithParams + ", " + concreteType)
      }
  }


  def collect[T](init: T, fct: (T, Formula) => T, f: Formula): T = {
    var acc = init
    val traverser = new Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        acc = fct(acc, f)
      }
    }
    traverser.traverse(f)
    acc
  }


  def collectTypes(f: Formula): Set[Type] =
    collect(Set[Type](), (acc: Set[Type], f: Formula) => acc + f.tpe, f)

  def collectSymbolsWithParams(f: Formula): Set[(Symbol, List[Type])] = {
    def process(acc: Set[(Symbol, List[Type])], f: Formula) = f match {
      case app @ Application(s, _) => acc + (s -> typeParams(app))
      case _ => acc
    }
    collect(Set[(Symbol, List[Type])](), process, f)
  }

  def collectSymbols(f: Formula): Set[Symbol] = {
    def process(acc: Set[Symbol], f: Formula) = f match {
      case app @ Application(s, _) => acc + s
      case _ => acc
    }
    collect(Set[Symbol](), process, f)
  }

  def nnf(f: Formula, neg: Boolean = false): Formula = f match {
    case Binding(ForAll, vs, f2) =>
      val bt = if (neg) Exists else ForAll
      Binding(bt, vs, nnf(f2, neg))
    case Binding(Exists, vs, f2) =>
      val bt = if (neg) ForAll else Exists
      Binding(bt, vs, nnf(f2, neg))
    case Application(Not, List(f2)) =>
      nnf(f2, !neg)
    case Application(And, args) =>
      val fct = if (neg) Or else And
      val args2 = args.map(nnf(_, neg))
      fct(args2:_*)
    case Application(Or, args) =>
      val fct = if (neg) And else Or
      val args2 = args.map(nnf(_, neg))
      fct(args2:_*)
    case other =>
      assert(other.tpe == Bool)
      if (neg) Not(other) else other
  }

  def normalize(f: Formula) = map(normalizef, f)
  private def normalizef(f: Formula): Formula = f match {
    case Implies(a,b) => Or(Not(a), b)
    case Geq(a,b) => Not(Lt(a,b))
    case Leq(a,b) => Not(Lt(b,a))
    case Gt(a,b) =>  Lt(b,a)
    case other => other
  }

  def simplifyBool(f: Formula): Formula = {
    def fct(f: Formula) = f match {
      case Or(lst @ _*) =>
        val lst2 = lst.toSet.filterNot(_ == False())
        if (lst2.exists(_ == True())) True()
        else if (lst2.isEmpty) False()
        else if (lst2.size == 1) lst2.head
        else Or(lst2.toList:_*)
      case And(lst @ _*) =>
        val lst2 = lst.toSet.filterNot(_ == True())
        if (lst2.exists(_ == False())) False()
        else if (lst2.isEmpty) True()
        else if (lst2.size == 1) lst2.head
        else And(lst2.toList:_*)
      case Not(Literal(b: Boolean)) =>
        Literal(!b)
      case other =>
        other
    }
    map(fct, f)
  }

}
