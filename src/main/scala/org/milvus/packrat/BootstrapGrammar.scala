package org.milvus.packrat

import java.io.File

import scala.collection.mutable
import scala.io.Source


sealed trait Expr

case class Sym(name: String) extends Expr

case class Terms(chars: String) extends Expr

case class Range(from: Char, to: Char) extends Expr

case class Opt(e: Expr) extends Expr

case class Alt(es: Expr*) extends Expr

case class Sq(es: Expr*) extends Expr

case class Star(e: Expr) extends Expr

case class Plus(e: Expr) extends Expr

case class Gap(e: Expr) extends Expr

case class Rl(lhs: Sym, rhs: Expr)


/**
  * The bootstrap grammar is a grammar that can read the actual PEGs. It is a PEG itself,
  * but it is specified in a simplified syntax that is easy to read with code. The
  * BootstrapGrammar parser reads the grammar specification from a file, and then instantiates
  * a PEG from it. The PEG can then be used with the packrat parser to read actual grammars.
  * 
  * <p>We read the bootstrap grammar with a super simple hand-written lexer/parser.
  */
object BootstrapGrammar {

  def readRulesFromFile(file: File): Seq[Rl] = {
    Source
      .fromFile(file)
      .getLines()
      .filter(_.trim.length > 0)
      .map(line2Rule)
      .toSeq
  }

  // In the bootstrap grammar, there is one rule per line. A rule is a non-terminal,
  // followed by an arrow, followed by an expression.
  def line2Rule(line: String): Rl = {
    val (start1, lhs) = readSymbol(line, consumeWS(line, 0))
    val start2 = consumeWS(line, start1)
    assert(line.charAt(start2) == '-')
    assert(line.charAt(start2 + 1) == '>')
    val (start3, expr) = readExpression(line, consumeWS(line, start2 + 2))
    val start4 = consumeWS(line, start3)
    // Make sure we consumed everything
    assert(start4 == line.length, s"Expected position ${line.length}, but found $start4")
    Rl(Sym(lhs), expr)
  }

  // An expression is either a character set, an operator with arguments, or a non-terminal
  def readExpression(s: String, pos: Int): (Int, Expr) = {
    assert(s.length > pos)
    s.charAt(pos) match {
      case '\'' =>
        val (i0, c0) = readCharacter(s, pos + 1)
        var i = i0
        var c = c0
        val sb = new StringBuilder()
        sb.append(c)
        while (s.charAt(i) != '\'') {
          val (i1, c1) = readCharacter(s, i)
          i = i1
          c = c1
          sb.append(c)
        }
        (i + 1, Terms(sb.toString))
      case _ =>
        val (next, sym) = readSymbol(s, pos)
        if (next >= s.length) {
          (next, Sym(sym))
        } else {
          if (s.charAt(next) == '(') sym match {
            case "Sq" =>
              val (i, args) = readArgs(s, next + 1)
              (i, Sq(args: _*))
            case "Alt" =>
              val (i, args) = readArgs(s, next + 1)
              (i, Alt(args: _*))
            case "Star" =>
              val (i, arg) = readArg(s, next + 1)
              (i, Star(arg))
            case "Opt" =>
              val (i, arg) = readArg(s, next + 1)
              (i, Opt(arg))
            case "Range" =>
              val (i, from, to) = readRangeArgs(s, next + 1)
              (i, Range(from, to))
          } else {
            (next, Sym(sym))
          }
        }
    }
  }

  // The arguments of a range operator are two characters.
  def readRangeArgs(s: String, pos: Int): (Int, Char, Char) = {
    val (start1, from) = readCharacter(s, consumeWS(s, pos))
    val start2 = consumeWS(s, start1)
    assert(s.charAt(start2) == ',')
    val (start3, to) = readCharacter(s, consumeWS(s, start2 + 1))
    val start4 = consumeWS(s, start3)
    assert(s.charAt(start4) == ')')
    (start4 + 1, from, to)
  }

  // Read an arbitrary number of arguments (sequences and disjunctions)
  def readArgs(s: String, pos: Int): (Int, Array[Expr]) = {
    def readArgs(i: Int, es: List[Expr]): (Int, List[Expr]) = {
      val (start1, expr) = readExpression(s, consumeWS(s, i))
      val start2 = consumeWS(s, start1)
      s.charAt(start2) match {
        case ')' => (start2 + 1, (expr :: es).reverse)
        case ',' => readArgs(start2 + 1, expr :: es)
        case c => throw new IllegalArgumentException(s"Unexpected character while reading arguments. Expected comma or closing paren, but found: $c")
      }
    }

    val (outPos, argList) = readArgs(pos, Nil)
    (outPos, argList.toArray)
  }

  // Read a single argument of a single-argument operator
  def readArg(s: String, pos: Int): (Int, Expr) = {
    val (start1, expr) = readExpression(s, consumeWS(s, pos))
    val start2 = consumeWS(s, start1)
    assert(s.charAt(start2) == ')')
    (start2 + 1, expr)
  }

  // Read a single character expression (without the quotes). A character is either
  // a plain character, or a backslash escaped character.
  def readCharacter(s: String, pos: Int): (Int, Char) = {
    var i = pos
    val c = s.charAt(i)
    val resChar: Char = c match {
      case '\\' => {
        i = i + 1
        val a = s.charAt(i)
        a match {
          case 'n' => '\n'
          case 't' => '\t'
          case _ => a
        }
      }
      case _ => c
    }
    (i + 1, resChar)
  }

  // Read a symbol. A symbol is either an Operator (followed by an open paren without
  // any intervening whitespace), or a non-terminal.
  def readSymbol(s: String, pos: Int): (Int, String) = {
    val buf = mutable.Buffer[Char]()
    var i = pos
    while (i < s.length && s.charAt(i).isLetter) {
      buf.append(s.charAt(i))
      i = i + 1
    }
    (i, new String(buf.toArray))
  }

  // Skip characters until we either hit the end of the string, or we find a non-whitespace
  // character.
  def consumeWS(s: String, pos: Int): Int = {
    var i = pos
    while (i < s.length && s.charAt(i).isWhitespace) {
      i = i + 1
    }
    i
  }
  
  def load(): Grammar = {
    val bsgName = "/BootstrapGrammar.peg"
    val is = BootstrapGrammar.getClass.getResourceAsStream(bsgName)
    val rules = Source
      .fromInputStream(is)
      .getLines()
      .filter(_.trim.length > 0)
      .map(line2Rule)
      .toSeq
    Grammar(rules)
  }

  def main(args: Array[String]): Unit = {
    val grammar = load()
    println(grammar)
  }

}
