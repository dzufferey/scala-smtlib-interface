package dzufferey.smtlib

import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Checks {

  def apply(f: Formula): Unit = {
    Logger("smtlib.checks", Debug, "sanity checks for " + f)
    val traverser = new FormulaUtils.Traverser {
      override def traverse(f: Formula) = {
        super.traverse(f)
        f.tpe match {
          case v @ TypeVariable(_) =>
            Logger.logAndThrow("smtlib.checks", Error, "type variables: " + v + " type of " + f)
          case Wildcard =>
            Logger.logAndThrow("smtlib.checks", Error, "wildcard type: " + f)
          case _ => ()
        }
      }
    }
    traverser.traverse(f)
  }

}
