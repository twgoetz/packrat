package org.milvus.packrat

sealed trait Expression

final case class Symbol(name: String, num: Int) extends Expression

final case class Terminal(ch: Char) extends Expression

case object EmptyExpression extends Expression

final case class Sequence(expressions: Seq[Expression]) extends Expression

final case class Alternative(expressions: Seq[Expression]) extends Expression

final case class KleeneClosure(expression: Expression) extends Expression

final case class Option(expression: Expression) extends Expression

final case class Rule(lhs: Symbol, rhs: Expression)

final case class Grammar(startSymbol: Symbol, rules: Seq[Rule]) {
  
  def getRule(symbol: Symbol): Rule = rules(symbol.num)

}
