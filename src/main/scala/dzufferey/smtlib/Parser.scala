package dzufferey.smtlib

import Names._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

class Lexer extends StdLexical {

  import scala.util.parsing.input.CharArrayReader.EofCh

  override def identChar = acceptIf({ c =>
    c != '(' && c != ')' && !c.isWhitespace && !c.isDigit
  })(el => "not in ident: " + el)

  override def whitespace: Parser[Any] = rep[Any](
      whitespaceChar
    | ';' ~ rep( chrExcept(EofCh, '\n', '\r') )
  )

  override def token: Parser[Token] = (
      '|' ~ rep( chrExcept('\\', '|') ) ~ '|' ^^ { case _ ~ chars ~ _ => StringLit("|" + chars.mkString("") + "|") }
    | super.token
  )

}

object Parser extends StandardTokenParsers {

  override val lexical = new Lexer

  lexical.delimiters += (
    "(", ")"
  )
  
  lexical.reserved += (
    "model",
    "assert",
    "declare-sort",
    "declare-fun",
    "define-sort",
    "define-fun",
    "forall",
    "exists",
    "ite",
    "true",
    "false",
    "!"
  )

  def paren[T](parser: Parser[T]): Parser[T] = "(" ~> parser <~ ")"

  def model: Parser[List[Command]] = paren("model" ~> rep(cmd))

  def getValueReply: Parser[List[(Formula, Formula)]] = paren(rep(assignement))

  def cmd: Parser[Command] = (
      paren("declare-sort" ~> ident ~ numericLit)                         ^^ { case id ~ num => DeclareSort(id, num.toInt) }
    | paren("declare-fun" ~> ident ~ paren(rep(sort)) ~ sort)             ^^ { case id ~ args ~ ret => DeclareFun(id, Function(args, ret))  }
    | paren("define-sort" ~> ident ~ paren(rep(ident)) ~ sort)            ^^ { case id ~ args ~ ret => DefineSort(id, args, ret) }
    | paren("define-fun" ~> ident ~ paren(rep(typedVar)) ~ sort ~ term)   ^^ { case id ~ vars ~ tpe ~ body => DefineFun(id, vars, tpe, body) }
    | paren("assert" ~> term)                                                         ^^ { f => Assert(f) }
    | term                                                                            ^^ { f => Assert(f) }
  )

  def assignement: Parser[(Formula, Formula)] = paren(term ~ term) ^^ { case t1 ~ t2 => (t1 -> t2)}

  def binder: Parser[(List[Variable], Formula) => Formula] = (
      "forall" ^^^ ( (vs: List[Variable], f: Formula) => ForAll(vs, f) )
    | "exists" ^^^ ( (vs: List[Variable], f: Formula) => Exists(vs, f) )
  )

  def tail: Parser[String] = (
    elem("tail", elt => elt.isInstanceOf[lexical.Identifier] && elt.chars.startsWith(".") ) ^^ (_.chars)
  )

  def number: Parser[Formula] = (
      numericLit ~ tail             ^^ { case h ~ t => Literal((h + t).toDouble) }
    | numericLit                    ^^ { str => Literal(str.toLong) }
  )

  def term: Parser[Formula] = (
      "true"                        ^^^ True()
    | "false"                       ^^^ False()
    | number
    | ident                         ^^ { id  => Variable(id) }
    | paren("ite" ~> rep(term))     ^^ { case args => Application(ite, args) }
    | paren(symbol ~ rep(term))     ^^ { case Minus ~ List(Literal(l: Long)) => Literal(-l)
                                         case Minus ~ List(Literal(d: Double)) => Literal(-d)
                                         case sym ~ args => Application(sym, args) }
    | paren(binder ~ paren(rep(typedVar)) ~ term) ^^ { case b ~ v ~ f => b(v, f) }
    | paren("!" ~> term ~ rep1(attribute)) ^^ { case t ~ attrs => t.setAttributes(attrs) }
    | paren(term)
  )

  def sort: Parser[Type] = (
      ident ^^ { case "Int" => Int
                 case "Bool" => Bool
                 case "Real" => Real
                 case id => UnInterpreted(id) }
    | paren(ident ~ rep(sort)) ^^ {
        case "Array" ~ List(s1, s2) => SArray(s1, s2)
        case id ~ args => sys.error("TODO parametric types")
      }
  )

  def symbol: Parser[Symbol] = (
    ident ^^ { id => InterpretedFct(id).orElse(DRealDecl(id)).getOrElse(UnInterpretedFct(id)) }
  )

  def typedVar: Parser[Variable] = "(" ~> ident ~ sort <~ ")" ^^ { case id ~ srt => Variable(id).setType(srt) }

  def keyword: Parser[String] = (
    elem("tail", elt => elt.isInstanceOf[lexical.Identifier] && elt.chars.startsWith(":") ) ^^ (_.chars)
  )

  def attributeTail: Parser[Attribute] = (
     stringLit ^^ { str => AttrSymbol("dummy", str) }
    | paren( rep(term) ) ^^ { exprs => AttrExpr("dummy", exprs) }
    | success( AttrKeyword("dummy") )
  )

  def attribute: Parser[Attribute] = keyword ~ attributeTail ^^ { case k ~ a => a.setKeyword(k) }

  def removeComments(str: String) = str.replaceAll("[ \t\f]*;.*(\\r\\n|\\r|\\n)", java.lang.System.lineSeparator())

  def parseTerm(str: String): Option[Formula] = {
    val tokens = new lexical.Scanner(str)
    val result = phrase(term)(tokens)
    if (result.successful) {
      val t = result.get
      Logger("smtlib.Parser", Debug, "smt term parsed:\n  " + t)
      Some(t)
    } else {
      Logger("smtlib.Parser", Warning, "parsing error: " + result.toString)
      None
    }
  }

  def parseModel(str: String): Option[List[Command]] = {
    val tokens = new lexical.Scanner(str)
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
  
  def parseGetValueReply(str: String): Option[List[(Formula, Formula)]] = {
    val noComments = removeComments(str)
    Logger("smtlib.Parser", Debug, "get value reply:\n" + noComments)
    val tokens = new lexical.Scanner(noComments)
    val result = phrase(getValueReply)(tokens)
    if (result.successful) {
      val assignments = result.get
      Logger("smtlib.Parser", Debug, "value parsed:\n  " + assignments.mkString("\n  "))
      Some(assignments)
    } else {
      Logger("smtlib.Parser", Warning, "parsing error: " + result.toString)
      None
    }
  }


}
