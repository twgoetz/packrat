package org.milvus.packrat

import org.milvus.packrat.samples.Tokenizer
import org.milvus.packrat.samples.grammar.{TokenizerParse, TokenizerSample}


trait Parser[ParseTree] {

  sealed abstract class ParseResult(val success: Boolean)

  case object ParseFailure extends ParseResult(false)

  case class ParseSuccess(pos: Int, parse: ParseTree) extends ParseResult(true)

  def parseLongest(from: Int, to: Int, input: Seq[Int]): ParseResult
  
  def parse(from: Int, to: Int, input: Seq[Int]): Option[ParseTree] = {
    parseLongest(from, to, input) match {
      case ParseFailure => None
      case ParseSuccess(pos, parse) =>
        if (pos == to) Some(parse)
        else None
    }
  }

  def parse(input: Seq[Int]): Option[ParseTree] = {
    parse(0, input.size, input)
  }
  
  def parseChars(input: Seq[Char]): Option[ParseTree] = {
    parse(input.map(_.toInt))
  }
  
}

object ParserMain extends App {
  
  sealed trait PrintNode
  case class Terminal(from: Int, to: Int) extends PrintNode
  case class NonTerminal(name: String, dtrs: Seq[PrintNode]) extends PrintNode
  
  def consolidateTerminals(nodes: Seq[PrintNode]): Seq[PrintNode] = {
    var out: List[PrintNode] = Nil
    var start = -1
    var end = -1
    for (node <- nodes) {
      node match {
        case Terminal(from, to) =>
          if (start < 0) {
            start = from
          }
          end = to
        case NonTerminal( _, _) =>
          if (start >= 0) {
            out = Terminal(start, end) :: out
            start = -1
          }
          out = node :: out
      }
    }
    if (start >= 0) out = Terminal(start, end) :: out
    out.reverse
  }
  
  def parseToPrintNode(parse: TokenizerParse): PrintNode = {
    parse match {
      case TokenizerSample.Position(n) => Terminal(n, n)
      case TokenizerSample.Node(name, dtrs @ _*) =>
        NonTerminal(name, consolidateTerminals(dtrs.map(parseToPrintNode)))
    }
  }
  
  def printTree(tokenizerParse: TokenizerParse): Unit = {
    val printTree = parseToPrintNode(tokenizerParse)
    //TODO: finish
    
  }
  
  val s = "This is a test."
  val result = TokenizerSample.parseChars(s)
  result match {
    case None => println("Parsing failed")
    case Some(parse) => println(parse)
  }
}