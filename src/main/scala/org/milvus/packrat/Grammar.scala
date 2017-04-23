package org.milvus.packrat

import java.util

import scala.collection.mutable

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

final case class Rule(lhs: Symbol, rhs: Expression)

class Grammar(rls: Seq[Rl]) {
  
  val symbolMap: mutable.Map[String, Symbol] = mutable.HashMap[String, Symbol]()
  
  val symbolList: mutable.Buffer[Symbol] = mutable.Buffer[Symbol]()
  
  val rules = rls.map(rl2rule).sortBy(_.lhs.num)
  
  rules.foreach(println)
  
  def startSymbol = symbolList(0)
  
  def getSymbolCode(s: String): Int = {
    symbolMap.get(s) match {
      case Some(x) => x.num
      case None => -1
    }
  }
  
  private def getOrAddSymbol(name: String): Symbol = {
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
  
  private def rl2rule(rl: Rl): Rule = {
    val lhs = getOrAddSymbol(rl.lhs.name)
    Rule(lhs, exp2expression(rl.rhs))
  }

  def exp2expression(expr: Expr): Expression = {
    expr match {
      case Sym(name) => {
        getOrAddSymbol(name)
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
      case sq: Sq => Sequence(sq.es.map(exp2expression))
      case alt: Alt => Alternative(alt.es.map(exp2expression))
    }
  }

  override 
  def toString: String = {
    rules.map(_.toString).mkString("\n")
  }
  
}

