package org.milvus.packrat

import scala.annotation.tailrec

sealed trait ParseResult

case object ParseFailure extends ParseResult

case class ParseSuccess(pos: Int, tree: ParseTree) extends ParseResult

object Parser {
  
  def parse(input: Seq[Char], grammar: Grammar): ParseTree = {
    val table = LookupTable(numCols = input.length, numSyms = grammar.rules.length)

    @tailrec
    def parseSequence(seq: Seq[Expression], seqIdx: Int, pos: Int, trees: Seq[ParseTree]): ParseResult = {
      if (seqIdx >= seq.length) {
        ParseSuccess(pos, ParseTreeNonTerminal(-1, trees))
      } else {
        val result = parse(seq(seqIdx), pos)
        result match {
          case ParseFailure => ParseFailure
          case ParseSuccess(next, tree) => parseSequence(seq, seqIdx + 1, next, trees ++ Seq(tree))
        }
      }
    }

    @tailrec
    def parseClosure(exp: Expression, pos: Int, trees: Seq[ParseTree]): ParseResult = {
        val result = parse(exp, pos)
        result match {
          case ParseFailure => ParseSuccess(pos, ParseTreeNonTerminal(-1, trees))
          case ParseSuccess(next, tree) => parseClosure(exp, next, trees ++ Seq(tree))
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
      if (pos >= input.length) {
        return ParseFailure
      }
      expression match {
        case Terminal(c) => {
          if (c == input(pos)) ParseSuccess(pos + 1, ParseTreeTerminal(c))
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
        case Option(expression) => {
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
        val catString = g.rules(cat).lhs.name
        s"[$catString $dtrsString]"
      }
      case ParseTreeTerminal(ch) => s"'$ch'"
    }
  }
  
}

case class ParseTreeNonTerminal(cat: Int, dtrs: Seq[ParseTree]) extends ParseTree

case class ParseTreeTerminal(ch: Char) extends ParseTree

object EmptyParseTree extends ParseTreeNonTerminal(-1, Seq())

case class LookupTable(numCols: Int, numSyms: Int) {
  
  private val table: Array[Array[(Int, ParseTree)]] = new Array(numSyms)
  
  for (i <- 0 until numSyms) table(i) = Array.fill[(Int, ParseTree)](numCols)(-1, EmptyParseTree)
  
  def isSet(pos: Int, sym: Int): Boolean = table(sym)(pos)._1 >= 0
  
  def set(from: Int, to: Int, sym: Int, tree: ParseTree): Unit = table(sym)(from) = (to, tree)
  
  def get(pos: Int, sym: Int): (Int, ParseTree) = table(sym)(pos)
}
