package dzufferey.smtlib

import Names._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Parser extends StandardTokenParsers {

  lexical.delimiters += (
    "(", ")", "!",
    "=", "<", ">", ">=", "<=", "=>",
    "+", "-", "*"
  )
  
  lexical.reserved += (
    "model", "assert",
    "declare", "define", "sort", "fun", 
    ":named",
    "forall", "exists",
    "ite",
    "true", "false"
  )

  override def ident = (
      super.ident ~ "!" ~ repsep(super.ident | numericLit, "!") ^^ { case head ~ _ ~ tail => if (tail.isEmpty) head else head + "!" + tail.mkString("!") }
    | super.ident 
  )
    
  def paren[T](parser: Parser[T]): Parser[T] = "(" ~> parser <~ ")"

  def model: Parser[List[Command]] = paren("model" ~> rep(cmd))

  def cmd: Parser[Command] = (
      paren("declare" ~> "-" ~> "sort" ~> ident ~ numericLit)                         ^^ { case id ~ num => DeclareSort(id, num.toInt) }
    | paren("declare" ~> "-" ~> "fun" ~> ident ~ paren(rep(sort)) ~ sort)             ^^ { case id ~ args ~ ret => DeclareFun(id, Function(args, ret))  }
    | paren("define" ~> "-" ~> "sort" ~> ident ~ paren(rep(ident)) ~ sort)            ^^ { case id ~ args ~ ret => DefineSort(id, args, ret) }
    | paren("define" ~> "-" ~> "fun" ~> ident ~ paren(rep(typedVar)) ~ sort ~ term)   ^^ { case id ~ vars ~ tpe ~ body => DefineFun(id, vars, tpe, body) }
    | paren("assert" ~> term)                                       ^^ { f => Assert(f) }
    | term                                                          ^^ { f => Assert(f) }
  )

  def binder: Parser[(List[Variable], Formula) => Formula] = (
      "forall" ^^^ ( (vs: List[Variable], f: Formula) => ForAll(vs, f) )
    | "exists" ^^^ ( (vs: List[Variable], f: Formula) => Exists(vs, f) )
  )

  def term: Parser[Formula] = (
      "true"                        ^^^ True()
    | "false"                       ^^^ False()
    | numericLit                    ^^ { str => Literal(str.toLong) }
    | ident                         ^^ { id  => Variable(id) }
    | paren("ite" ~> rep(term))     ^^ { case args => Application(ite, args) }
    | paren(symbol ~ rep(term))     ^^ { case sym ~ args => Application(sym, args) }
    | paren(binder ~ paren(rep(typedVar)) ~ term) ^^ { case b ~ v ~ f => b(v, f) }
    | paren("!" ~> term ~ (":named" ~> ident)) ^^ { case t ~ id => t } //TODO ??
  )

  def sort: Parser[Type] = (
      ident ^^ { case "Int" => Int
                 case "Bool" => Bool
                 case id => UnInterpreted(id) }
    | paren(ident ~ rep(sort)) ^^ { case id ~ args => sys.error("TODO FSet, FOption, Product") }
  )

  def symbol: Parser[Symbol] = (
      "="  ^^^ Eq
    | "<"  ^^^ Lt
    | ">"  ^^^ Gt
    | ">=" ^^^ Geq
    | "<=" ^^^ Leq
    | "=>" ^^^ Implies
    | "+"  ^^^ Plus
    | "-"  ^^^ Minus
    | "*"  ^^^ Times
    | ident ^^ { id => InterpretedFct(id).getOrElse(UnInterpretedFct(id)) }
  )

  def typedVar: Parser[Variable] = "(" ~> ident ~ sort <~ ")" ^^ { case id ~ srt => Variable(id).setType(srt) }

  def removeComments(str: String) = str.replaceAll("[ \t\f]*;;.*\\n", "")

  def parseModel(str: String): Option[List[Command]] = {
    val noComments = removeComments(str)
    Logger("smtlib.Parser", Debug, "raw smt model:\n" + noComments)
    val tokens = new lexical.Scanner(noComments)
    val result = phrase(model)(tokens)
    if (result.successful) {
      val cmds = result.get
      Logger("smtlib.Parser", Debug, "smt command parsed:\n  " + cmds.mkString("\n  "))
      Some(cmds)
    } else {
      Logger("smtlib.Parser", Warning, "parsing error: " + result.toString)
      None
    }
  }

}
