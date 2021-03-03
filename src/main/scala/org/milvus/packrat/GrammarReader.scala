package org.milvus.packrat

import java.io.InputStream
import java.nio.charset.StandardCharsets

import org.milvus.packrat.samples.grammar.GrammarParser.Position
import org.milvus.packrat.samples.grammar.{GrammarParse, GrammarParser}

import scala.annotation.tailrec
//import scala.collection.convert.WrapAsJava.`deprecated asJavaCollection`
import scala.io.{Codec, Source}

/**
  * Main entry point for end user grammars.
  */
object GrammarReader {

  //TODO read different versions of grammars here (e.g., support for rich lexical items).

  private lazy val standardBSG = BootstrapGrammar.load

  def read(is: InputStream): Grammar = {
    Grammar(readGrammar(is))
  }

  def parseGrammar(is: InputStream): Seq[Rl] = {
    val grammarString = Source.fromInputStream(is, Codec.UTF8.name).mkString
    val parseResult = GrammarParser.parseChars(grammarString.toCharArray.toSeq)
    parseResult match {
      case None =>
        println("Parse failure")
        Seq()
      case Some(parse) =>
        parse2Rules(parse, grammarString)
    }
  }

  def parse2Rules(parse: GrammarParse, text: String): Seq[Rl] = {
    parse match {
      case GrammarParser.Grammar(sq@_*) => sq.map(rule2rule(_, text))
      case _ =>
        println("WTF?")
        Seq()
    }
  }

  def rule2rule(ruleParse: GrammarParse, text: String): Rl = {
    println(ruleParse)
    ruleParse match {
      case GrammarParser.Rule(dtrs@_*) =>
        val lhs = sym2sym(dtrs(1), text)
        val rhs = ex2ex(dtrs(6), text)
        Rl(lhs, rhs)
      case _ => Rl(Sym("WTF?"), Sq())
    }
  }

  def ex2ex(exParse: GrammarParse, text: String): Expr = {
    exParse match {
      case GrammarParser.Symbol(_*) => sym2sym(exParse, text)
      case GrammarParser.Expr(dtrs@_*) =>
        val subExprs = dtrs
          .filter({
            case GrammarParser.OptWS(_*) => false
            case _ => true
          })
          .map(ex2ex(_, text))
        if (subExprs.size == 1) subExprs(0)
        else Sq(subExprs: _*)
      case GrammarParser.SeqElementExpr(dtrs@_*) =>
        val subExprs = dtrs
          .filter({
            case GrammarParser.OptWS(_*) => false
            case GrammarParser.AltOperator(_*) => false
            case _ => true
          })
          .map(ex2ex(_, text))
        if (subExprs.size == 1) subExprs(0)
        else Alt(subExprs: _*)
      case GrammarParser.AltElementExpr(dtrs@_*) =>
        val subEx = ex2ex(dtrs(0), text)
        if (dtrs.size == 1) subEx
        else {
          val opChar = op2op(dtrs(2), text)
          opChar match {
            case '?' => Opt(subEx)
            case '*' => Star(subEx)
            case '+' => Plus(subEx)
            case _ => Sym("WTF")
          }
        }
      case GrammarParser.OperandExpr(ex) => ex2ex(ex, text)
      case GrammarParser.ParenExpr(dtrs@_*) => ex2ex(dtrs(2), text)
      case GrammarParser.SimpleExpr(ex) => ex2ex(ex, text)
      case GrammarParser.GapExpr(dtrs@_*) => Gap(ex2ex(dtrs(2), text))
      case GrammarParser.Terminal(dtrs@_*) => ex2ex(dtrs(1), text)
      case GrammarParser.Position(pos) => Terms(text(pos).toString)
      case GrammarParser.EscapedChar(dtrs@_*) =>
        val pos = getOffset(dtrs(1))
        val ch = text(pos) match {
          case 'n' => '\n'
          case 't' => '\t'
          case c => c
        }
        Terms(ch.toString)
      case GrammarParser.CharSet(dtrs@_*) =>
        val chars = dtrs.tail.dropRight(1)
          .map(ex2ex(_, text))
          .map({
            case Terms(str) => str
            case _ => ""
          })
          .mkString
        Terms(chars)
      case GrammarParser.Hex(pos) => Terms(text(getOffset(pos)).toString)
      case GrammarParser.HexChar(dtrs@_*) =>
        val hexChars = dtrs.drop(2)
          .map(ex2ex(_, text))
          .map({
            case Terms(str) => str(0)
            case _ => '0'
          })
        val ints = for (i <- 0 until hexChars.size) yield {
          val multiplier = 16 * (i + 1)
          val value: Int = {
            val c = hexChars(hexChars.size - i - 1)
            c match {
              case '0' => 0
              case '1' => 1
              case '2' => 2
              case '3' => 3
              case '4' => 4
              case '5' => 5
              case '6' => 6
              case '7' => 7
              case '8' => 8
              case '9' => 9
              case 'a' => 10
              case 'b' => 11
              case 'c' => 12
              case 'd' => 13
              case 'e' => 14
              case 'f' => 15
              case 'A' => 10
              case 'B' => 11
              case 'C' => 12
              case 'D' => 13
              case 'E' => 14
              case 'F' => 15
            }
          }
          value * multiplier
        }
        Terms(ints.sum.toChar.toString())
      case GrammarParser.Range(dtrs@_*) =>
        val fromChar = ex2ex(dtrs(0), text) match {
          case Terms(str) => str(0)
          case _ => ' '
        }
        val toChar = ex2ex(dtrs(5), text) match {
          case Terms(str) => str(0)
          case _ => ' '
        }
        Range(fromChar, toChar)
      case parse => throw new RuntimeException(s"Unknown grammar construct: $parse")
    }
  }

  def op2op(opPars: GrammarParse, text: String): Char = {
    opPars match {
      case GrammarParser.Operator(op) =>
        op match {
          case GrammarParser.Position(pos) => text(pos)
          case _ => 'x'
        }
      case _ => 'x'
    }
  }

  def sym2sym(symParse: GrammarParse, text: String): Sym = {
    symParse match {
      case GrammarParser.Symbol(offsets@_*) =>
        val start = getOffset(offsets(0))
        val end = getOffset(offsets.last) + 1
        Sym(text.substring(start, end))
      case _ => Sym("WTF?")
    }
  }

  def getOffset(position: GrammarParse): Int = {
    position match {
      case GrammarParser.Position(pos) => pos
      case _ => -1
    }
  }

  def readGrammar(is: InputStream): Seq[Rl] = {
    val grammarString = Source.fromInputStream(is, StandardCharsets.UTF_8.name()).mkString
    val parseTree = ParserInterpreter.parse(grammarString, standardBSG)
    tree2rules(parseTree)
  }

  // Read a parse tree as obtained by BootstrapGrammar.peg and transform it into a sequence of rules (which we can then
  // use to instantiate a grammar). We do this extra step because it is easier to create a grammar from the intermediate
  // Rl format, than from a parse tree directly. We can afford to do this since this is a one time process and not
  // performance critical. Also, it is highly likely that the parse tree format will change in the future. The Rl type
  // on the other hand should remain fairly constant.
  private def tree2rules(tree: ParseTree): Seq[Rl] = {
    tree match {
      case ParseTreeNonTerminal(cat, dtrs) => {
        if (cat != standardBSG.getSymbolCode("Grammar")) {
          throw new IllegalArgumentException(s"Grammar error: Grammar expected, but found $tree")
        }
        println("Parsing successful!")
        println(tree.toString(standardBSG))
        trees2rules(dtrs)
      }
      case _ => throw new IllegalArgumentException(s"Grammar error: Grammar expected, but found $tree")
    }
  }

  private def trees2rules(trees: Seq[ParseTree]): Seq[Rl] = {
    trees.map(tree2rule)
  }

  private def tree2rule(tree: ParseTree): Rl = {
    tree match {
      case ParseTreeNonTerminal(_, dtrs) => // Only rules possible at this point
        val lhsPos = getLhs(dtrs)
        val rhsPos = getRhs(dtrs, lhsPos)
        val lhs = dtrs(lhsPos) match {
          case ParseTreeNonTerminal(_, terminals) => terminals
            .map({
              case ParseTreeTerminal(terminal) => terminal.toChar
              case _ => ' '
            })
            .mkString("")
          case _ => ""
        }
        Rl(Sym(lhs), tree2expr(dtrs(rhsPos)).get)
      case _ => throw new IllegalArgumentException(s"Grammar error: Rule expected, but found $tree")
    }
  }

  private def tree2expr(tree: ParseTree): Option[Expr] = {
    val out = tree2expr(tree, None)
    out match {
      case Some(Sq(es@_*)) =>
        if (es.size == 1) Some(es(0))
        else out
      case _ => out
    }
  }

  private def tree2expr(tree: ParseTree, prev: Option[Expr]): Option[Expr] = {
    tree match {
      case ParseTreeTerminal(ch) => Some(Terms(ch.toChar.toString))
      case ParseTreeNonTerminal(cat, dtrs) =>
        standardBSG.getSymbolForId(cat).get.name match {
          case "Expr" =>
            val out = trees2seq(dtrs)
            if (out.size == 1) Some(out(0))
            else Some(Sq(out: _*))
          case "Operator" =>
            tree2expr(dtrs(0)).get match {
              case Terms(ch) => ch match {
                case "?" => Some(Opt(prev.get))
                case "*" => Some(Star(prev.get))
                case "+" => Some(Plus(prev.get))
              }
              case _ => None
            }
          case "GapExpr" => Some(Gap(tree2expr(dtrs(2)).get))
          case "ParenExpr" =>
            if (dtrs.size == 1) tree2expr(dtrs(0))
            else tree2expr(dtrs(2))
          case "Alternative" =>
            if (dtrs.size > 3) Some(Alt(prev.get, tree2expr(dtrs(3)).get))
            else None
          case "SeqExpr" =>
            val exprSeq = trees2seq(dtrs)
            if (exprSeq.size == 1) Some(exprSeq(0)) else Some(Sq(exprSeq: _*))
          case "SimpleExpr" =>
            val exprSeq = trees2seq(dtrs)
            if (exprSeq.size == 1) Some(exprSeq(0)) else Some(Sq(exprSeq: _*))
          case "Symbol" =>
            val name = dtrs
              .map({
                case ParseTreeTerminal(ch) => ch.toChar.toString
                case _ => ""
              })
              .mkString("")
            Some(Sym(name))
          case "OptWS" => None
          case "Terminal" => tree2expr(dtrs(1))
          case "CharSet" =>
            val chars = dtrs
              .take(dtrs.size - 1)
              .tail
              .map({
                case ParseTreeTerminal(ch) => ch.toChar.toString
                case ParseTreeNonTerminal(_, terms) => {
                  val value = tree2expr(terms(0))
                  if (value.isDefined) {
                    value.get match {
                      case Terms(s) => s
                      case _ => ""
                    }
                  } else ""
                }
                case _ => ""
              })
              .mkString("")
            Some(Terms(chars))
          case "Range" =>
            val start = dtrs(0) match {
              case ParseTreeNonTerminal(_, term) => term(1) match {
                case ParseTreeTerminal(ch) => ch
                case _ => 0
              }
              case _ => 0
            }
            val end = dtrs(5) match {
              case ParseTreeNonTerminal(_, term) => term(1) match {
                case ParseTreeTerminal(ch) => ch
                case _ => 0
              }
              case _ => 0
            }
            Some(Range(start.toChar, end.toChar))
          case "EscapedChar" =>
            tree2expr(dtrs(1))
          case "HexChar" =>
            val hexString = dtrs
              .flatMap({
                case ParseTreeNonTerminal(_, Seq(ParseTreeTerminal(ch))) => Some(ch.toChar.toString)
                case _ => None
              })
              .mkString("")
            Some(Terms(Integer.parseInt(hexString, 16).toChar.toString))
          case s: String => throw new IllegalArgumentException(s"Unknown category: $s")
        }

    }
  }

  private def trees2seq(trees: Seq[ParseTree]): Seq[Expr] = {

    @tailrec
    def trees2seq(prev: Option[Expr], pos: Int, result: Seq[Expr]): Seq[Expr] = {
      if (pos >= trees.size) {
        prev match {
          case None => result
          case Some(expr) => result :+ expr
        }
      } else {
        tree2expr(trees(pos), prev) match {
          case None => trees2seq(prev, pos + 1, result)
          case Some(expr) => expr match {
            case Star(_) => trees2seq(Some(expr), pos + 1, result)
            case Plus(_) => trees2seq(Some(expr), pos + 1, result)
            case Opt(_) => trees2seq(Some(expr), pos + 1, result)
            case Alt(_*) => trees2seq(Some(expr), pos + 1, result)
            case _ => prev match {
              case None => trees2seq(Some(expr), pos + 1, result)
              case Some(e) => trees2seq(Some(expr), pos + 1, result :+ e)
            }
          }
        }
      }
    }

    trees2seq(None, 0, Seq())
  }

  private def getLhs(trees: Seq[ParseTree]): Int = {
    for (i <- 0 until trees.size) {
      trees(i) match {
        case ParseTreeNonTerminal(lhs, _) => if (lhs == standardBSG.getSymbolCode("Symbol")) return i
        case _ => // keep going
      }
    }
    trees.size
  }

  private def getRhs(trees: Seq[ParseTree], pos: Int): Int = {
    for (i <- (pos + 1) until trees.size) {
      trees(i) match {
        case ParseTreeNonTerminal(lhs, _) => if (lhs == standardBSG.getSymbolCode("Expr")) return i
        case _ => // keep going
      }
    }
    trees.size
  }

  def main(args: Array[String]): Unit = {
    val inputStream = classOf[Grammar].getClassLoader.getResourceAsStream("samples/test.peg")
    val grammar = read(inputStream)
    println(grammar)
  }

}
