package dzufferey.smtlib

import dzufferey.utils._
import dzufferey.utils.LogLevel._
import scala.sys.process._
import java.io._
import scala.collection.mutable.{HashSet, Stack}
import java.util.concurrent.TimeUnit

class DRealHackI(th: Theory,
                 cmd: String,
                 options: Iterable[String],
                 precision: Option[Double],
                 implicitDeclaration: Boolean,
                 incremental: Boolean,
                 dumpToFile: Option[String]) {


  protected val stack = Stack[List[Formula]]()
  protected var current = List[Formula]()

  def push {
    stack.push(current)
    current = Nil
  }
  
  def pop {
    current = stack.pop
  }

  def assert(f: Formula) {
    current ::= f
  }

  def checkSat: Result = {
    val solver = new DRealHack(th, cmd, options, precision, implicitDeclaration, incremental, dumpToFile)
    solver.test(current ++ stack.toList.flatten)
  }
}


class DRealHack( th: Theory,
                 cmd: String,
                 options: Iterable[String],
                 precision: Option[Double],
                 implicitDeclaration: Boolean,
                 incremental: Boolean,
                 dumpToFile: Option[String]) {

  protected var stackCounter = 0

  SysCmd.acquire
  protected var released = false

  //////////////
  // Plumbing //
  //////////////

  protected val solver = java.lang.Runtime.getRuntime.exec(Array(cmd) ++ options, null, null)
  protected val solverInput = {
    val out = solver.getOutputStream()
    //val out = new org.apache.commons.io.output.TeeOutputStream(solver.getOutputStream(), System.out)
    new BufferedWriter(new OutputStreamWriter(out))
  }
  protected val solverOutput = new BufferedReader(new InputStreamReader(solver.getInputStream()))
  protected val solverError = new BufferedReader(new InputStreamReader(solver.getErrorStream()))

  protected val fileDump = dumpToFile.map( file => new BufferedWriter(new FileWriter(file)) )

  //////////////////
  // Declarations //
  //////////////////

  protected val declaredV = HashSet[Variable]()
  protected val declStack = Stack(Set[Variable]())
  
  protected val declaredS = HashSet[(Symbol, List[Type])]()
  protected val symbolStack = Stack(Set[(Symbol, List[Type])]())
  
  protected val declaredT = HashSet[Type]()
  protected val typeStack = Stack(Set[Type]())

  ////////////////////
  // Initialisation //
  ////////////////////

  Logger("smtlib", Debug, "starting: " + (Array(cmd) ++ options).mkString(" "))
  toSolver(SetOption("print-success", false))
  toSolver(SetOption("produce-models", true))
  if (precision.isDefined) {
    toSolver(SetOption("precision", precision.get))
  }
  toSolver("(set-logic "+th+")")

  //default declarations
  declaredT ++= th.declaredSorts
  declaredS ++= th.declaredFunctions.map(t => (t, Nil))

  ///////////////
  ///////////////

  override def finalize {
    try {
      solver.exitValue
      fileDump.foreach(_.close)
    } catch {
      case _: java.lang.IllegalThreadStateException =>
        solver.destroy
    } finally {
      if (!released) {
        SysCmd.release
        released = true
      }
    }
  }

  protected def toSolver(cmd: String) {
    Logger("smtlib", Debug, "> " +cmd)
    solverInput.write(cmd)
    solverInput.newLine
    solverInput.flush
    for (f <- fileDump) {
      f.write(cmd)
      f.newLine
      f.flush
    }
  }
  
  protected def toSolver(cmd: Command) {
    Logger("smtlib", Debug, "> " +cmd)
    Printer(solverInput, cmd)
    solverInput.newLine
    solverInput.flush
    for (f <- fileDump) {
      Printer(f, cmd)
      f.newLine
      f.flush
    }
  }

  protected def fromSolver(timeout: Long = 10000): String = {

    def reader(stream: BufferedReader) =
      new java.util.concurrent.Callable[String] {
        def call = {
          val acc = new StringBuilder()
          do {
            val line = stream.readLine
            Logger("smtlib", Debug, "< " + line)
            acc.append(line)
            acc.append("\n")
          } while(stream.ready)
          acc.toString.trim
        }
      }

    if (solverError.ready) {
      val acc = new StringBuilder()
      while(solverError.ready) {
        acc.append(solverError.readLine)
        acc.append("\n")
      }
      Logger.logAndThrow("smtlib", Error, "solver returned:\n" + acc)
    } else {
      val future = Solver.executor.submit(reader(solverOutput))
      try {
        val res = future.get(timeout, TimeUnit.MILLISECONDS)
        res
      } catch {
        case e: java.util.concurrent.TimeoutException =>
          Logger("smtlib", Warning, "solver timeout.")
          forceExit
          "TIMEOUT"
      }
    }
  }

  def forceExit {
    try {
      solver.destroy 
      solverInput.close
      solverOutput.close
      solverError.close
      for (f <- fileDump) f.close
    } finally {
      if (!released) {
        SysCmd.release
        released = true
      }
    }
  }

  def declare(t: Type) = {
    try {
      toSolver(DeclareSort(Names.tpe(t), Names.tpeArity(t)))
    } catch {
      case e: Exception =>
        //collectTypes returns a bit more than what it should
        Logger("smtlib", Warning, "unsupported type: " + t)
    }
  }

  def typeDecl(t: Type) = {
    val (args, ret) = t match {
      case Function(args, r) => (args, r)
      case other => (Nil, other)
    }
    val argsDecl = args.map(Names.tpe).mkString("("," ",")")
    argsDecl + " " + Names.tpe(ret)
  }

  def declare(f: Formula) = f match {
    case Variable(v) => toSolver(DeclareFun(v, f.tpe))
    case other => Logger.logAndThrow("smtlib", Error, "not supported: " + other)
  }
  
  def declare(sp: (Symbol, List[Type])) = {
    val (s, params) = sp
    s match {
      case Eq => //has a special status
        ()
      case UnInterpretedFct(f, t, p) =>
        Logger.assert(t.isDefined, "smtlib", "declaring sym with unknown type: " + f)
        val name = Names.overloadedSymbol(s, params)
        val tpe = s.instanciateType(params)
        try {
          toSolver(DeclareFun(name, tpe))
        } catch {
          case e: Exception =>
            //collectSymbolsWithParams returns a bit more than what it should
            Logger.logAndThrow("smtlib", Error, "unsupported type: " + name + ": " + tpe + " from " + f + ": " + t + p.mkString("[",",","]") + " and " + params)
        }
      case i: InterpretedFct =>
        val name = Names.overloadedSymbol(s, params)
        val tpe = s.instanciateType(params)
        toSolver(DeclareFun(name, tpe))
    }
  }

  protected def pushOnStack[A](elts: Set[A], stack: Stack[Set[A]], decls: HashSet[A]): Set[A] = {
    val newElts = elts -- decls
    decls ++= newElts
    val frame = stack.pop
    stack.push(frame ++ newElts)
    newElts
  }

  def mkDeclarations(f: Formula) = {
    val newSort = pushOnStack(FormulaUtils.collectTypes(f), typeStack, declaredT)
    newSort foreach declare
    val newSym = pushOnStack(FormulaUtils.collectSymbolsWithParams(f), symbolStack, declaredS)
    newSym foreach declare
    val newVars = pushOnStack(f.freeVariables, declStack, declaredV)
    newVars foreach declare
  }
  
  def assert(f: Formula) {
    if (implicitDeclaration) {
      mkDeclarations(f)
    }
    toSolver(Assert(f))
  }

  def testB(f: Formula): Boolean = {
    test(f) match {
      case Sat(_) => true
      case UnSat => false
      case _ => sys.error("result is not (un)sat.")
    }
  }

  def test(f: Formula): Result = {
    test(FormulaUtils.getConjuncts(f))
  }

  def test(conjuncts: List[Formula]): Result = {
    conjuncts.foreach(Checks(_))
    conjuncts.foreach(assert(_))
    checkSat()
  }

  def declareODE(name: String, formula: Formula) {
    if (implicitDeclaration) {
      mkDeclarations(formula)
      pushOnStack(Set(Variable(name).setType(Real)), declStack, declaredV)
    }
    formula match {
      case Eq(Application(DRealDecl.timeDerivative, List(Variable(lhs))), rhs) =>
        val cmd = "(define-ode " + name + " ((= d/dt["+lhs+"] " + Printer.toString(rhs) + ")))"
        toSolver(cmd)
      case other =>
        Logger.logAndThrow("smtlib", Error, "not an ODE: " + other)
    }
  }

  def pintegrate( minTime: Formula, maxTime: Formula,
                  preVars: List[Variable], postVars: List[Variable],
                  holders: List[Variable]) {
    val mit = Printer.toString(minTime)
    val mat = Printer.toString(maxTime)
    val pre = preVars.map(Printer.toString).mkString("["," ","]")
    val post = postVars.map(Printer.toString).mkString("["," ","]") 
    val hld = holders.map(Printer.toString).mkString("["," ","]")
    val cmd = "(assert (= "+post+" (pintegral "+mit+" "+mat+" "+pre+" "+hld+")))"
    toSolver(cmd)
  }

  def assertForallT(mode: Int, minTime: Formula, maxTime: Formula, formula: Formula) {
    val mit = Printer.toString(minTime)
    val mat = Printer.toString(maxTime)
    val f = Printer.toString(formula)
    val cmd = "(assert (forall_t "+mode+" ["+mit+" "+mat+"] "+f+"))"
    toSolver(cmd)
  }

  def checkSat(timeout: Long = 10000): Result = {
    toSolver(CheckSat)
    toSolver(Exit)
    solverInput.close
    val res = fromSolver(timeout) match {
      case "sat" =>
        //get the model from stderr
        val acc = new StringBuilder()
        while(solverError.ready) {
          acc.append(solverError.readLine)
          acc.append("\n")
        }
        val model = DRealParser.parse(acc.toString).map( lst => {
          val lst2 = lst.map{ case (v,l,u) => v -> ValD((l+u) / 2) }.toMap
          new Model(Map(), lst2, Map())
        })
        Sat(model)
      case "unsat" => UnSat
      case "unknown" => Unknown
      case other =>
        Logger("smtlib", Warning, "checkSat: solver said " + other)
        Failure(other)
    }
    try {
      solver.waitFor
      solverOutput.close
      solverError.close
      for (f <- fileDump) f.close
    } finally {
      if (!released) {
        SysCmd.release
        released = true
      }
    }
    res
  }

}

object DReal {
  
  val solver = "dReal"
  val solverArg = Array[String]()
  
  def apply(th: Theory, precision: Double) = {
    assert(th == QF_NRA || th == QF_NRA_ODE)
    new DRealHack(th, solver, solverArg, Some(precision), true, false, None)
  }
  
  def apply(th: Theory, precision: Double, file: String) = {
    assert(th == QF_NRA || th == QF_NRA_ODE)
    new DRealHack(th, solver, solverArg, Some(precision), true, false, Some(file))
  }


  def apply(th: Theory) = {
    assert(th == QF_NRA || th == QF_NRA_ODE)
    new DRealHack(th, solver, solverArg, None, true, false, None)
  }
  
  def apply(th: Theory, file: String) = {
    assert(th == QF_NRA || th == QF_NRA_ODE)
    new DRealHack(th, solver, solverArg, None, true, false, Some(file))
  }


  def incremental(th: Theory) = {
    assert(th == QF_NRA || th == QF_NRA_ODE)
    new DRealHackI(th, solver, solverArg, None, true, false, None)
  }
  
  def incremental(th: Theory, file: String) = {
    assert(th == QF_NRA || th == QF_NRA_ODE)
    new DRealHackI(th, solver, solverArg, None, true, false, Some(file))
  }

}

object DRealParser extends scala.util.parsing.combinator.RegexParsers {

  def nonWhite: Parser[String] = """[^\s,]+""".r ^^ { _.toString }

  def nonEq: Parser[String] = """[^=]+""".r ^^ { _.toString }

  def number: Parser[String] = """-?(\d+(\.\d*)?)([eE][+-]?\d+)?""".r

  def range: Parser[(String, Double, Double)] = (
    nonWhite ~ (":" ~> nonEq ~> "=" ~> "[" ~> number) ~ ("," ~> number <~ "]") ^^ { case id ~ lb ~ ub => (id, lb.toDouble, ub.toDouble) }
  | nonWhite <~ ":" <~ nonEq <~ "=" <~ "[ -INFTY ]" ^^ { case id => (id, Double.NegativeInfinity, Double.NegativeInfinity) }
  | nonWhite <~ ":" <~ nonEq <~ "=" <~ "[ INFTY ]" ^^ { case id => (id, Double.PositiveInfinity, Double.PositiveInfinity) }
  )

  def ranges = rep(range)

  def whole = "Solution:" ~> ranges

  def parse(str: String): Option[List[(Variable, Double, Double)]] = {
    val result = parseAll(whole, str)
    if (result.successful) {
      val cmds = result.get
      Some(cmds.map{ case (a,b,c) => (Variable(a).setType(Real), b, c)})
    } else {
      None
    }
  }

}
