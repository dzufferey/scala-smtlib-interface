package dzufferey.smtlib

import dzufferey.utils.Misc
import dzufferey.utils._
import dzufferey.utils.LogLevel._

sealed abstract class Def {
  def tpe: Type
}
sealed abstract class ValDef extends Def
case class ValD(d: Double) extends ValDef {
  override def toString = d.toString
  def tpe: Type = Real
}
case class ValI(i: Long) extends ValDef {
  override def toString = i.toString
  def tpe: Type = Int
}
case class ValB(b: Boolean) extends ValDef {
  override def toString = b.toString
  def tpe: Type = Bool
}
case class ValExt(idx: Int, tp: Type) extends ValDef {
  override def toString = tp.toString + "!" + idx
  def tpe: Type = tp
}
case class FunDef(defs: List[(List[ValDef], ValDef)], default: ValDef) extends Def {
  def tpe: Type = { //TODO does not take polymorphism into account
    defs.headOption match {
      case Some((args, ret)) => Function(args.map(_.tpe), ret.tpe)
      case None => default.tpe //FIXME not really
    }
  }
}

object Def {

  def eval(f: FunDef,  args: List[ValDef]): ValDef = {
    def same(c: List[ValDef]): Boolean = {
      c.zip(args).forall{ case (a,b) => a == b }
    }
    f.defs.find{ case (args, _) => same(args) } match {
      case Some(s) => s._2
      case None => f.default
    }
  }
  
  def uneval(domains: Map[Type, Set[ValExt]], d: Def, ret: ValDef): List[List[ValDef]] = d match {
    case v: ValDef =>
      if (v == ret) List(Nil) else Nil
    case f: FunDef =>
      val mtch = f.defs.filter(_._2 == ret).map(_._1)
        if (!mtch.isEmpty)
          mtch
        else {
          if (ret == f.default) {
            val args = f.defs.head._1.map{
              case ValB(_) => List(ValB(true), ValB(false))
              case ValI(_) => sys.error("cannot complement integers")
              case ValD(_) => sys.error("cannot complement floating points")
              case ValExt(_, tpe) => domains(tpe).toList
            }
            val allArgs = Misc.cartesianProduct(args)
            allArgs.flatMap( seq => {
              val lst = seq.toList
              if (f.defs.exists( _._1 == lst )) None
              else Some(lst)
            }).toList
          } else {
            Nil
          }
        }
  }

}


class Model(domains: Map[Type, Set[ValExt]],
            constants: Map[Variable, ValDef],
            functions: Map[Symbol, Def])
{

  def get(s: Symbol, args: ValDef*): Option[ValDef] = {
    val aLst = args.toList
    functions.get(s).map( _ match {
      case v: ValDef => v
      case f: FunDef => Def.eval(f, aLst)
    })
  }

  def apply(s: Symbol, args: ValDef*): ValDef = get(s, args:_*).get
  
  def apply(v: Variable): ValDef = constants(v)

  override def toString = {
    val buffer = new StringBuilder
    buffer.append("model\n")
    buffer.append("  domains:\n")
    for ( (t, vals) <- domains ) {
      buffer.append("    " + t + ": " + vals.mkString(", "))
      buffer.append("\n")
    }
    buffer.append("  constants:\n")
    for ( (c, dc) <- constants) {
      buffer.append("    " + c + " = " + dc + "\n")
    }
    buffer.append("  functions:\n")
    for ( (f, df) <- functions) {
      df match {
        case v: ValDef =>
          buffer.append("    " + f + " = " + v + "\n")
        case FunDef(defs, default) =>
          for ((args, v) <- defs) {
          buffer.append("    " + f + args.mkString("(",", ",") = ") + v + "\n")
          }
          buffer.append("    " + f + "(_) = " + default + "\n")
      }
    }
    buffer.toString
  }

}

object Model {

  def apply(cmds: List[Command], variables: Iterable[Variable], declared: Iterable[(Symbol, List[Type])]) = {

    val values: Map[String, ValExt] = (cmds.collect{
      case DeclareFun(id, Function(Nil, tpe)) =>
        (id ->  ValExt(id.split("!").last.toInt, tpe)) //z3 values
    }).toMap
    val domains = values.values.groupBy(_.tpe).map{ case (k, v) => (k, v.toSet) }

    val toSym = declared.foldLeft(Map[String, Symbol]())( (acc, decl) => {
      acc + (symFullName(decl._1, decl._2) -> decl._1)
    })
    //println("toSym: " + toSym)

    def tryParseVal(f: Formula): Option[ValDef] = f match {
      case Literal(b: Boolean) => Some(ValB(b))
      case Literal(l: Long) => Some(ValI(l))
      case Minus(Literal(l: Long)) => Some(ValI(-l))
      case Variable(id) if id.startsWith("@uc_") => //cvc4 values
        val idx = id.reverse.takeWhile(_.isDigit).reverse
        val tpe = id.substring(4, id.length - idx.length - 1) match {
          case "Int" => Int
          case "Bool" => Bool
          case id => UnInterpreted(id)
        }
        Some(ValExt(idx.toInt, tpe))
      case Variable(id) => values get id
      case _ => None
    }
 
    def parseCase(args: Formula, ret: Formula): (List[ValDef], ValDef) = {
      val args2 = args match {
        case And(cs @ _*) => cs.toList
        case other => List(other)
      }
      val args3 = args2.map( _ match {
        case eq @ Eq(v1, v2) => tryParseVal(v2).orElse(tryParseVal(v1)).getOrElse(sys.error("could not parse: " + eq.toStringFull))
        case other => sys.error("expected Eq, found: " + other)
      })
      val (args4, retParsed) = ret match {
        case eq @ Eq(v1, v2) => tryParseVal(v2).orElse(tryParseVal(v1)) match {
            case Some(vd) => (args3 :+ vd, ValB(true)) //TODO forgetting the false case!!
            case None => sys.error("could not parse: " + eq.toStringFull)
          }
        case ret =>
          val r = tryParseVal(ret).getOrElse(sys.error("cannot parse (expected value) " + ret))
          (args3, r)
      }
      (args4, retParsed)
    }

    def getSym(id: String): Symbol = {
      if (toSym contains id) toSym(id) else UnInterpretedFct(id)
    }
 
    def tryParseFun(d: DefineFun): Option[(Symbol, Def)] = {
      val sym = getSym(d.id)
      val (cases, default) = collectCases(d.body)
      tryParseVal(default).map( v => {
        val cases2 = cases map { case (args, v) => parseCase(args, v) }
        (sym, if (cases2.isEmpty) v else FunDef(cases2, v) )
      })
    }

    var rest = cmds collect { case d: DefineFun => d }
    var defs = Map.empty[Symbol,Def]

    def loop(fct: DefineFun => Option[(Symbol, Def)]): Unit = {
      var progress = !rest.isEmpty
      while(progress) {
        progress = false
        val r = rest
        rest = Nil
        for (d <- r) {
          fct(d) match {
            case Some(d) =>
              defs = defs + d
              progress = true
            case None =>
             rest = d :: rest
          }
        }
      }
    }

    //first pass
    loop(tryParseFun)
      
    def inline(symbol: Symbol, args: List[Option[Symbol]]): Def = {
      defs(symbol) match {
        case v: ValDef => v
        case FunDef(cases, default) =>
          def invert(vals: List[ValDef]): List[List[ValDef]] = {
            val vals2 = args.zip(vals).map{
              case (Some(s), v) => Def.uneval(domains, defs(s), v).flatten //assume single arg
              case (None, v) => List(v)
            }
            val cart = Misc.cartesianProduct(vals2)
            cart.toList.map(_.toList)
          }
          val cases2 = cases.flatMap( c => {
            val lst = invert(c._1)
            lst.map(_ -> c._2)
          })
          FunDef(cases2, default)
      }
    }

    def tryFillDef(d: DefineFun): Option[(Symbol, Def)] = {
      try {
        d.body match {
          case Application(UnInterpretedFct(s, _, _), args) =>
            val args2 = args map {
              case Application(sym, List(_)) => Some(sym)
              case Variable(_) => None
              case _ => sys.error("??")
            }
            Some(getSym(d.id) -> inline(getSym(s), args2))
          case _ => sys.error(d.id + " body is " + d.body)
        }
      } catch {
        case e: Exception =>
          None
      }
    }

    //second pass for functions defined with other funs ...
    loop(tryFillDef)
    
    def symEval(symbol: Symbol, args: List[ValDef]): ValDef = {
      defs(symbol) match {
        case v: ValDef => v
        case f: FunDef => Def.eval(f, args)
      }
    }

    def fEval(f: Formula, params: Map[String,ValDef]): ValDef = f match {
      case Literal(b: Boolean) => ValB(b)
      case Literal(l: Long) => ValI(l)
      case Literal(d: Double) => ValD(d)
      case Variable(id) => 
        if (values contains id) values(id)
        else params(id)
      case Eq(e1, e2) =>
        val v1 = fEval(e1, params)
        val v2 = fEval(e2, params)
        ValB(v1 == v2)
      case Application(s @ (Leq | Geq | Lt | Gt), List(e1, e2)) =>
        val v1 = fEval(e1, params)
        val v2 = fEval(e2, params)
        (v1, v2) match {
          case (ValI(i1), ValI(i2)) =>
            s match {
              case Leq => ValB(i1 <= i2)
              case Geq => ValB(i1 >= i2)
              case Lt =>  ValB(i1 < i2)
              case Gt =>  ValB(i1 > i2)
              case _ => sys.error("?!") //removing this line make the scala compiler run out of memory
            }
          case _ => sys.error("expected two ValI: " + v1 + ", " + v2)
        }
      case Application(UnInterpretedFct("ite",_,_), List(cnd, tr, fa)) =>
        val eCnd = fEval(cnd, params)
        val eTr = fEval(tr, params)
        val eFa = fEval(fa, params)
        eCnd match {
          case ValB(b) =>
            if (b) eTr else eFa
          case _ => sys.error("expected ValB: " + eCnd)
        }
      case Application(UnInterpretedFct(s,_,_), args) =>
        val eArgs = args.map(fEval(_, params))
        symEval(getSym(s), eArgs)
      case _ => sys.error("did not expect: " + f)
    }

    def generateArgs(args: List[Variable]): List[Map[String,ValDef]] = args match {
      case x :: xs =>
        val ys = generateArgs(xs)
        val d = domains(x.tpe)
        ys.flatMap( m => d.map( v => m + (x.name -> v) ) )
      case Nil => List(Map.empty[String,ValDef])
    }

    def tryEvalDef(d: DefineFun): Option[(Symbol, Def)] = {
      try {
        val paramss = generateArgs(d.args)
        val sym = getSym(d.id)
        val cases = for ( params <- paramss) yield {
          val args = d.args.map( v => params(v.name) )
          val res = fEval(d.body, params)
          (args, res)
        }
        val default = cases.head._2
        Some(sym -> FunDef(cases, default))
      } catch {
        case e: Exception =>
          None
      }
    }
    
    //third pass: brute force on the parameter space
    loop(tryEvalDef)

    if (!rest.isEmpty) {
      sys.error("cannot reconstruct model: " + rest)
    }
    
 
    //remove the fct introduced by the solver
    val defined = toSym.values.toSet
    val there = defs filter { case (s, _) => defined(s) }

    var vs = variables.map( v => v.name -> v).toMap
    val vars = defs collect { case (UnInterpretedFct(v,_,_), d: ValDef) if vs contains v => vs(v) -> d }

    new Model(domains, vars, there)
  }

  private def symFullName(sym: Symbol, tpes: List[Type]): String = Names.overloadedSymbol(sym, tpes)

  private def collectCases(f: Formula): (List[(Formula, Formula)], Formula) = f match {
    case Application(UnInterpretedFct("ite",_,_), List(cnd, tr, fa)) =>
      val (acc, other) = collectCases(fa)
      ((cnd, tr) :: acc, other)
    case other => (Nil, other)
  }
  
  private def tryParseVal(f: Formula): Option[ValDef] = f match {
    case Literal(b: Boolean) => Some(ValB(b))
    case Literal(l: Long) => Some(ValI(l))
    case Minus(Literal(l: Long)) => Some(ValI(-l))
    case Variable(id) if id.startsWith("@uc_") => //cvc4 values
      val idx = id.reverse.takeWhile(_.isDigit).reverse
      val tpe = id.substring(4, id.length - idx.length - 1) match {
        case "Int" => Int
        case "Bool" => Bool
        case id => UnInterpreted(id)
      }
      Some(ValExt(idx.toInt, tpe))
    case Variable(id) if id.split("!").length == 3 =>  //z3 value
      val parts = id.split("!")
      assert(parts(1) == "val")
      val tpe = UnInterpreted(parts(0))
      val idx = parts(2).toInt
      Some(ValExt(idx, tpe))
    case other =>
      Logger("smtlib", Debug, "tryParseVal: failed to parse value from " + other)
      None
  }

  private def tryGetFunDef( solver: Solver,
                            domains: Map[Type, Set[ValDef]],
                            repr: Map[ValDef,Formula],
                            sym: Symbol,
                            args: List[Type]
                          ): (Option[FunDef], Map[ValDef,Formula])  = {
    var res: List[(List[ValDef], ValDef)] = Nil
    var repr2: Map[ValDef, Formula] = Map.empty
    val sym2 = UnInterpretedFct(symFullName(sym, args))
    def mkArgs(tpes: List[Type], stack: List[ValDef]): Unit = tpes match {
      case t :: ts =>
        domains.getOrElse(t, Set[ValDef]()).foreach( vd => mkArgs(ts, vd :: stack) )
      case Nil =>
        val args = stack.reverse
        val f = Application(sym2, args.map(repr))
        solver.getValue(f) match {
          case Some((_,f2) :: _) =>
            for (ret <- tryParseVal(f2)) {
              res ::= (args -> ret)
              if (!repr.contains(ret)) {
                repr2 += (ret -> f)
              }
            }
          case _ => 
            Logger("smtlib", Debug, "tryGetFunDef: failed to parse get value for " + f)
        }
    }
    val ofd = sym.instanciateType(args) match {
      case Function(argsT, ret) =>
        mkArgs(argsT, Nil)
        Some(FunDef(res, ValExt(-1, ret)))
      case _ => None
    }
    (ofd, repr2)
  }

  private def mergeFD(d1: FunDef, d2: FunDef): FunDef = {
    FunDef((d1.defs ++ d2.defs).toSet.toList, d1.default)
  }

  //declared is the type params for polymorphic functions
  //this might not finish if you have number in your model
  def getPartialModel(solver: Solver,
                      variables: Iterable[Variable],
                      declared: Iterable[(Symbol, List[Type])]
                     ): Option[Model] = {
    try {
      solver.getValue(variables.toSeq: _*).map( lst => {
        val constants = Map(lst.map{ case (id,v) => (id.asInstanceOf[Variable],tryParseVal(v).get)}: _*)
        var extendedDomains: Map[Type, Set[ValDef]] = constants.values.groupBy(_.tpe).foldLeft(Map.empty[Type, Set[ValDef]])( (acc,kv) => acc + (kv._1 -> kv._2.toSet) )
        var repr: Map[ValDef,Formula] = constants.map{ case (a,b) => (b,a) }
        var newRepr = true
        var functions: Map[Symbol,FunDef] = Map.empty
        while (newRepr) {
          newRepr = false
          for ( (sym,args) <- declared ) {
            val (ofd, repr2) = tryGetFunDef(solver, extendedDomains, repr, sym, args)
            for (fd <- ofd) {
              val newFd = if (functions contains sym) mergeFD(functions(sym), fd) else fd
              functions += (sym -> newFd)
            }
            if (!repr2.isEmpty) {
              newRepr = true
              repr ++= repr2
              for (v <- repr2.keys) {
                val values: Set[ValDef] = extendedDomains.getOrElse(v.tpe, Set.empty) + v
                extendedDomains += (v.tpe -> values)
              }
            }
          }
        }
        val domains = extendedDomains.filter{
          case (UnInterpreted(_), _) => true
          case _ => false
        }.asInstanceOf[Map[Type,Set[ValExt]]]
        new Model(domains, constants, functions)
      })
    } catch {
      case err: Exception =>
        Logger("smtlib", Debug, "getPartialModel: " + err)
        None
    }
  }

}
