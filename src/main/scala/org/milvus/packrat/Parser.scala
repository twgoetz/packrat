package org.milvus.packrat

import org.milvus.packrat.samples.Tokenizer


trait Parser[ParseTree] {

  sealed abstract class ParseResult(val success: Boolean)

  case object ParseFailure extends ParseResult(false)

  case class ParseSuccess(pos: Int, parse: ParseTree) extends ParseResult(true)

  def parseLongest(from: Int, to: Int, input: Seq[Int]): ParseResult
  
  def codeForExternalSymbol(name: String): Int
  
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
  val s = "This is a test."
  val result = Tokenizer.parseChars(s)
  result match {
    case None => println("Parsing failed")
    case Some(parse) => println(parse)
  }
}