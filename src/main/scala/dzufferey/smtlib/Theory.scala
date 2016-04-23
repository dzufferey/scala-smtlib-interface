package dzufferey.smtlib

abstract class Theory {
  def declaredSorts: Set[Type] = Set(Bool)
  def declaredFunctions: Set[Symbol] = Set(And, Or, Not, Eq, Implies)
}

case object QF_UF extends Theory
case object UF extends Theory

case object QF_LIA extends Theory {
  override def declaredSorts: Set[Type] = super.declaredSorts + Int
  override def declaredFunctions: Set[Symbol] = super.declaredFunctions ++ Set(Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt)
}
case object LIA extends Theory {
  override def declaredSorts: Set[Type] = super.declaredSorts + Int
  override def declaredFunctions: Set[Symbol] = super.declaredFunctions ++ Set(Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt)
}

case object QF_UFLIA extends Theory {
  override def declaredSorts: Set[Type] = super.declaredSorts + Int
  override def declaredFunctions: Set[Symbol] = super.declaredFunctions ++ Set(Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt)
}
case object UFLIA extends Theory {
  override def declaredSorts: Set[Type] = super.declaredSorts + Int
  override def declaredFunctions: Set[Symbol] = super.declaredFunctions ++ Set(Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt)
}

case object AUFLIA extends Theory {
  override def declaredSorts: Set[Type] = super.declaredSorts + Int + SArray(Int,Int)
  override def declaredFunctions: Set[Symbol] = super.declaredFunctions ++ Set(Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt, Select, Store)
}

/** for dreal */
case object QF_NRA extends Theory {
  override def declaredSorts: Set[Type] = super.declaredSorts + Real
  override def declaredFunctions: Set[Symbol] = super.declaredFunctions ++ Set(Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt) ++ DRealDecl.fcts
}
/** for dreal */
case object QF_NRA_ODE extends Theory {
  override def declaredSorts: Set[Type] = super.declaredSorts + Real
  override def declaredFunctions: Set[Symbol] = super.declaredFunctions ++ Set(Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt) ++ DRealDecl.fcts ++ DRealDecl.ode
}

/* dreal supported function */
object DRealDecl {
  //TODO redefine Plus, Minus, Times, Divides, Leq, Lt, Geq, Gt over Real ?
  val pow  =  new UnInterpretedFct( "^", Some(Real ~> Real ~> Real))
  val sqrt =  new UnInterpretedFct( "sqrt", Some(Real ~> Real))
  val exp  =  new UnInterpretedFct( "exp", Some(Real ~> Real))
  val log  =  new UnInterpretedFct( "log", Some(Real ~> Real))
  val sin  =  new UnInterpretedFct( "sin", Some(Real ~> Real))
  val cos  =  new UnInterpretedFct( "cos", Some(Real ~> Real))
  val tan  =  new UnInterpretedFct( "tan", Some(Real ~> Real))
  val sinh =  new UnInterpretedFct( "sinh", Some(Real ~> Real))
  val cosh =  new UnInterpretedFct( "cosh", Some(Real ~> Real))
  val tanh =  new UnInterpretedFct( "tanh", Some(Real ~> Real))
  val asin =  new UnInterpretedFct( "asin", Some(Real ~> Real))
  val acos =  new UnInterpretedFct( "acos", Some(Real ~> Real))
  val atan =  new UnInterpretedFct( "atan", Some(Real ~> Real))
  val atan2 = new UnInterpretedFct( "atan2", Some(Real ~> Real ~> Real))
  val asinh = new UnInterpretedFct( "asinh", Some(Real ~> Real))
  val acosh = new UnInterpretedFct( "acosh", Some(Real ~> Real))
  val atanh = new UnInterpretedFct( "atanh", Some(Real ~> Real))
  val abs =   new UnInterpretedFct( "abs", Some(Real ~> Real))

  val fcts = Set( pow, sqrt, exp, log,
                  sin, cos, tan,
                  sinh, cosh, tanh,
                  asin, acos, atan, atan2,
                  asinh, acosh, atanh )

  val timeDerivative = new UnInterpretedFct( "d/dt", Some(Real ~> Real))
  val ode = Set(
    timeDerivative
    //TODO derivative in other variables
  )

  def apply(str: String): Option[UnInterpretedFct] = {
    fcts.find( f => f.symbol == str )
  }
}
