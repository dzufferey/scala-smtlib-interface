package dzufferey.smtlib

sealed abstract class Command

case class DeclareSort(id: String, arity: Int) extends Command
case class DeclareFun(id: String, sig: Type) extends Command
case class DefineSort(id: String, args: List[String], ret: Type) extends Command
case class DefineFun(id: String, args: List[Variable], ret: Type, body: Formula) extends Command

case class Assert(f: Formula) extends Command
case object CheckSat extends Command
case object Exit extends Command

case object Push extends Command
case object Pop extends Command

case object GetModel extends Command
case class GetValue(terms: List[Formula]) extends Command

case class SetOption(option: String, value: Any) extends Command
