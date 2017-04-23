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

case class Rl(lhs: Sym, rhs: Expr)

object BootstrapGrammar {

  def readRulesFromFile(file: File): Seq[Rl] = {
    Source
      .fromFile(file)
      .getLines()
      .filter(_.trim.length > 0)
      .map(line2Rule)
      .toSeq
  }

  def line2Rule(line: String): Rl = {
    val start1 = consumeWS(line, 0)
    val (start2, lhs) = readSymbol(line, start1)
    val start3 = consumeWS(line, start2)
    assert(line.charAt(start3) == '-')
    assert(line.charAt(start3 + 1) == '>')
    val start4 = consumeWS(line, start3 + 2)
    val (start5, expr) = readExpression(line, start4)
    val start6 = consumeWS(line, start5)
    assert(start6 == line.length, s"Expected position ${line.length}, but found $start6")
    Rl(Sym(lhs), expr)
  }

  def readExpression(s: String, pos: Int): (Int, Expr) = {
    assert(s.length > pos)
    s.charAt(pos) match {
      case '\'' => {
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
      }
      case _ => {
        val (next, sym) = readSymbol(s, pos)
        if (s.charAt(next) == '(') {
          sym match {
            case "Sq" => {
              val (i, args) = readArgs(s, next + 1)
              (i, Sq(args: _*))
            }
            case "Alt" => {
              val (i, args) = readArgs(s, next + 1)
              (i, Alt(args: _*))
            }
            case "Star" => {
              val (i, arg) = readArg(s, next + 1)
              (i, Star(arg))
            }
            case "Opt" => {
              val (i, arg) = readArg(s, next + 1)
              (i, Opt(arg))
            }
            case "Range" => {
              val (i, from, to) = readRangeArgs(s, next + 1)
              (i, Range(from, to))
            }
          }
        } else {
          (next, Sym(sym))
        }
      }
    }
  }

  def readRangeArgs(s: String, pos: Int): (Int, Char, Char) = {
    val start1 = consumeWS(s, pos)
    val (start2, from) = readCharacter(s, start1)
    val start3 = consumeWS(s, start2)
    assert(s.charAt(start3) == ',')
    val start4 = consumeWS(s, start3 + 1)
    val (start5, to) = readCharacter(s, start4)
    val start6 = consumeWS(s, start5)
    assert(s.charAt(start6) == ')')
    (start6 + 1, from, to)
  }

  def readArgs(s: String, pos: Int): (Int, Array[Expr]) = {
    def readArgs(i: Int, es: List[Expr]): (Int, List[Expr]) = {
      val start0 = consumeWS(s, i)
      val (start1, expr) = readExpression(s, start0)
      val start2 = consumeWS(s, start1)
      s.charAt(start2) match {
        case ')' => (start2 + 1, es.reverse)
        case ',' => readArgs(start2 + 1, expr :: es)
      }
    }

    val (outPos, argList) = readArgs(pos, Nil)
    (outPos, argList.toArray)
  }

  def readArg(s: String, pos: Int): (Int, Expr) = {
    val start0 = consumeWS(s, pos)
    val (start1, expr) = readExpression(s, start0)
    val start2 = consumeWS(s, start1)
    assert(s.charAt(start2) == ')')
    (start2 + 1, expr)
  }

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

  def readSymbol(s: String, pos: Int): (Int, String) = {
    val buf = mutable.Buffer[Char]()
    var i = pos
    while (i < s.length && s.charAt(i).isLetter) {
      buf.append(s.charAt(i))
      i = i + 1
    }
    (i, new String(buf.toArray))
  }

  def consumeWS(s: String, pos: Int): Int = {
    var i = pos
    while (i < s.length && s.charAt(i).isWhitespace) {
      i = i + 1
    }
    i
  }

  def main(args: Array[String]): Unit = {
    val bsgName = "/BootstrapGrammar.peg"
    val is = BootstrapGrammar.getClass.getResourceAsStream(bsgName)
    val rules = Source
      .fromInputStream(is)
      .getLines()
      .filter(_.trim.length > 0)
      .map(line2Rule)
      .toSeq
    val grammar = new Grammar(rules)
    println(grammar)
  }

}
