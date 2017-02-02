package dzufferey.smtlib

// representing attributes that may be associated with formula like names and pattern

sealed abstract class Attribute {
  def keyword: String
  def setKeyword(k: String): Attribute
}

case class AttrKeyword(keyword: String) extends Attribute {
  def setKeyword(k: String) = AttrKeyword(k)
}

case class AttrSymbol(keyword: String, content: String) extends Attribute {
  def setKeyword(k: String) = AttrSymbol(k, content)
}

case class AttrExpr(keyword: String, exprs: Seq[Formula]) extends Attribute {
  def setKeyword(k: String) = AttrExpr(k, exprs)
}
