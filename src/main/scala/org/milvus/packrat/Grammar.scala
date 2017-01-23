package org.milvus.packrat

final case class Symbol(name: String, num: Int)

final case class Rule(lhs: Symbol, rhs: Expression)

sealed class Expression

final case class Terminal(ch: Char) extends Expression

final case object EmptyExpression extends Expression

final case class Sequence(expressions: Seq[Expression]) extends Expression

final case class Alternative(expressions: Seq[Expression]) extends Expression

final case class KleeneClosure(expression: Expression) extends Expression

final case class Grammar(rules: Seq[Rule]) {

}
