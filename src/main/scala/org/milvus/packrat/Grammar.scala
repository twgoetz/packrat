package org.milvus.packrat

import java.util

import scala.collection.mutable

// These are the Expressions used in our grammars internally, the ones that the parser knows how to handle.
sealed trait Expression

final case class Symbol(name: String, num: Int) extends Expression

final case class Terminal(ch: Int) extends Expression {
  override def toString: String = {
    val arg = ch.toChar
    s"Terminal($arg)"
  }
}

final case class Terminals(char: Array[Int]) extends Expression {
  override def toString: String = {
    val args = char.map(_.toChar).mkString(", ")
    s"Terminals($args)"
  }
}

final case class TerminalRange(from: Int, to: Int) extends Expression

case object EmptyExpression extends Expression

final case class Sequence(expressions: Seq[Expression]) extends Expression

final case class Alternative(expressions: Seq[Expression]) extends Expression

final case class KleeneClosure(expression: Expression) extends Expression

final case class TransitiveClosure(expression: Expression) extends Expression

final case class Optional(expression: Expression) extends Expression

final case class ReluctantGap(expression: Expression) extends Expression

final case class Rule(lhs: Symbol, rhs: Expression)

class Grammar {

  private val symbolMap: mutable.Map[String, Symbol] = mutable.HashMap[String, Symbol]()

  private val symbolList: mutable.Buffer[Symbol] = mutable.Buffer[Symbol]()

  val rules: mutable.Buffer[Rule] = mutable.Buffer[Rule]()

  rules.foreach(println)

  def startSymbol = symbolList(0)

  def getSymbolCode(s: String): Int = {
    symbolMap.get(s) match {
      case Some(x) => x.num
      case None => -1
    }
  }

  def getSymbolForId(id: Int): Option[Symbol] = {
    if (id >= 0 && id < symbolList.size) Some(symbolList(id)) else None
  }

  def getOrAddSymbol(name: String): Symbol = {
    symbolMap.get(name) match {
      case None => {
        val symbol = Symbol(name, symbolList.length)
        symbolList.append(symbol)
        symbolMap.put(name, symbol)
        symbol
      }
      case Some(symbol) => symbol
    }
  }

  override
  def toString: String = {
    rules.map(_.toString).mkString("\n")
  }

}

object Grammar {

  def apply(rls: Seq[Rl]): Grammar = {
    val grammar = new Grammar
    rls.map(rl2rule(_, grammar))
      .sortBy(_.lhs.num)
      .foreach(grammar.rules += _)
    grammar
  }

  private def rl2rule(rl: Rl, grammar: Grammar): Rule = {
    def exp2expression(expr: Expr): Expression = {
      expr match {
        case Sym(name) => {
          grammar.getOrAddSymbol(name)
        }
        case Terms(chars) => {
          if (chars.length == 1) {
            Terminal(chars.charAt(0))
          } else {
            val intArray = Util.string2IntArray(chars)
            util.Arrays.sort(intArray)
            Terminals(intArray)
          }
        }
        case Range(from, to) => TerminalRange(from, to)
        case Opt(e) => Optional(exp2expression(e))
        case Star(e) => KleeneClosure(exp2expression(e))
        case Plus(e) => TransitiveClosure(exp2expression(e))
        case Gap(e) => ReluctantGap(exp2expression(e))
        case sq: Sq => Sequence(sq.es.map(exp2expression))
        case alt: Alt => Alternative(alt.es.map(exp2expression))
      }
    }

    val lhs = grammar.getOrAddSymbol(rl.lhs.name)
    Rule(lhs, exp2expression(rl.rhs))

  }


}
