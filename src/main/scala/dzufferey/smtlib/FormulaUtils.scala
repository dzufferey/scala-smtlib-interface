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
    case And(lst) => lst.flatMap(getConjuncts)
    case other => List(other)
  }

  def typeParams(app: Application): List[Type] = app.fct match {
    case Eq | And | Or | Plus | Times => //skip those: overloaded in smtlib
      Nil
    case normal =>
      val params = normal.typeParams
      val concreteType = Function(app.args.map(_.tpe), app.tpe)
      val subst = Type.unify(normal.typeWithParams, concreteType)
      subst match {
        case Some(s) => params.map(s)
        case None => sys.error("FormulaUtils.typeWithParams, cannot unify: " + normal.typeWithParams + ", " + concreteType)
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

}
