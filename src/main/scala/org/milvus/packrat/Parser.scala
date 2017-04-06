package org.milvus.packrat

import java.util

import scala.annotation.tailrec

sealed trait ParseResult

case object ParseFailure extends ParseResult

case class ParseSuccess(pos: Int, tree: ParseTree) extends ParseResult

object Parser {
  
  val SEQ = -1
  
  def parse(text: String, grammar: Grammar): ParseTree = {
    parse(Util.string2IntArray(text), grammar)
  }
  
  def parse(input: Seq[Int], grammar: Grammar): ParseTree = {
    val table = LookupTable(numCols = input.length + 1, numSyms = grammar.rules.length)

    @tailrec
    def parseSequence(seq: Seq[Expression], seqIdx: Int, pos: Int, trees: Seq[ParseTree]): ParseResult = {
      if (seqIdx >= seq.length) {
        ParseSuccess(pos, ParseTreeNonTerminal(SEQ, trees))
      } else {
        val result = parse(seq(seqIdx), pos)
        result match {
          case ParseFailure => ParseFailure
          case ParseSuccess(next, tree) => {
            tree match {
              case ParseTreeNonTerminal(SEQ, dtrs) => parseSequence(seq, seqIdx + 1, next, trees ++ dtrs)
              case _ => parseSequence(seq, seqIdx + 1, next, trees ++ Seq(tree))
            }
            
          }
        }
      }
    }

    @tailrec
    def parseClosure(exp: Expression, pos: Int, trees: Seq[ParseTree]): ParseResult = {
      val result = parse(exp, pos)
      result match {
        case ParseFailure => ParseSuccess(pos, ParseTreeNonTerminal(SEQ, trees))
        case ParseSuccess(next, tree) => {
          tree match {
            case ParseTreeNonTerminal(SEQ, dtrs) => parseClosure(exp, next, trees ++ dtrs)
            case _ => parseClosure(exp, next, trees ++ Seq(tree))
          }
          
        }
      }

    }

    def parsePlus(exp: Expression, pos: Int, trees: Seq[ParseTree]): ParseResult = {
      val result = parse(exp, pos)
      result match {
        case ParseFailure => ParseFailure
        case ParseSuccess(next, tree) => {
          tree match {
            case ParseTreeNonTerminal(SEQ, dtrs) => parseClosure(exp, next, trees ++ dtrs)
            case _ => parseClosure(exp, next, trees ++ Seq(tree))
          } 
        }
      }

    }

    @tailrec
    def parseAlternative(seq: Seq[Expression], seqIdx: Int, pos: Int): ParseResult = {
      if (seqIdx >= seq.length) {
        ParseFailure
      } else {
        val result = parse(seq(seqIdx), pos)
        result match {
          case ParseFailure => parseAlternative(seq, seqIdx + 1, pos)
          case ParseSuccess(_, _) => result
        }
      }
    }
    
    def parse(expression: Expression, pos: Int): ParseResult = {
      expression match {
        case Terminal(c) => {
          if (pos >= input.length) {
            return ParseFailure
          }
          if (c == input(pos)) ParseSuccess(pos + 1, ParseTreeTerminal(c))
          else ParseFailure
        }
        case Terminals(ints) => {
          if (pos >= input.length) {
            return ParseFailure
          }
          val c = input(pos)
          if (util.Arrays.binarySearch(ints, c) >= 0) ParseSuccess(pos + 1, ParseTreeTerminal(c))
          else ParseFailure
        }
        case TerminalRange(from, to) => {
          if (pos >= input.length) {
            return ParseFailure
          }
          val c = input(pos)
          if (c >= from && c <= to) ParseSuccess(pos + 1, ParseTreeTerminal(c))
          else ParseFailure
        }
        case Symbol(_, idx) => {
          val (toPos, tree) = table.get(pos = pos, sym = idx)
          if (toPos >= 0) ParseSuccess(toPos, tree)
          else {
            val success = parse(grammar.rules(idx).rhs, pos)
            success match {
              case ParseSuccess(next, tree) => {
                val outTree = tree match {
                  case ParseTreeTerminal(_) => ParseTreeNonTerminal(idx, Seq(tree))
                  case ParseTreeNonTerminal(_, dtrs) => ParseTreeNonTerminal(idx, dtrs)
                }
                table.set(from = pos, to = next, idx, outTree)
                ParseSuccess(next, outTree)
              }
              case ParseFailure => success
            }
          }
        }
        case EmptyExpression => ParseSuccess(pos, EmptyParseTree)
        case Sequence(seq) => parseSequence(seq, seqIdx = 0, pos = pos, Seq())
        case Alternative(seq) => parseAlternative(seq, seqIdx = 0, pos = pos)
        case KleeneClosure(expression) => parseClosure(expression, pos, Seq())
        case TransitiveClosure(expression) => parsePlus(expression, pos, Seq())
        case Optional(expression) => {
          val result = parse(expression, pos)
          result match {
            case ParseFailure => ParseSuccess(pos, EmptyParseTree)
            case _ => result
          }
        }
      }
      
    }
    
    parse(grammar.startSymbol, pos = 0) match {
      case ParseFailure => EmptyParseTree
      case ParseSuccess(endPos, tree) => if (endPos == input.length) tree else EmptyParseTree
    }
  }
  
}

sealed trait ParseTree {
  
  def toString(g: Grammar): String = {
    this match {
      case EmptyParseTree => "[]"
      case ParseTreeNonTerminal(cat, dtrs) => {
        val dtrsString = dtrs
          .map(_.toString(g))
          .mkString(" ")
        val catString = if (cat >= 0) g.symbolList(cat).name else "SEQ"
        s"[$catString $dtrsString]"
      }
      case ParseTreeTerminal(ch) => s"'$ch'"
    }
  }
  
}

case class ParseTreeNonTerminal(cat: Int, dtrs: Seq[ParseTree]) extends ParseTree

case class ParseTreeTerminal(ch: Int) extends ParseTree

object EmptyParseTree extends ParseTreeNonTerminal(Parser.SEQ, Seq())

case class LookupTable(numCols: Int, numSyms: Int) {
  
  private val table: Array[Array[(Int, ParseTree)]] = new Array(numSyms)
  
  for (i <- 0 until numSyms) table(i) = Array.fill[(Int, ParseTree)](numCols)(Parser.SEQ, EmptyParseTree)
  
  def isSet(pos: Int, sym: Int): Boolean = table(sym)(pos)._1 >= 0
  
  def set(from: Int, to: Int, sym: Int, tree: ParseTree): Unit = table(sym)(from) = (to, tree)
  
  def get(pos: Int, sym: Int): (Int, ParseTree) = table(sym)(pos)
}

object Util {

  def string2IntArray(s: String): Array[Int] = {
    val result = new Array[Int](s.length)
    for (i <- 0 until s.length) result(i) = s.charAt(i)
    result
  }
  
}