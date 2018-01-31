package org.milvus.packrat.samples

import org.milvus.packrat.Parser

import scala.collection.mutable

sealed trait TokenizerParse

case class Position(pos: Int) extends TokenizerParse

case class WordOrPunct(cats: TokenizerParse*) extends TokenizerParse

case class Word(cats: TokenizerParse*) extends TokenizerParse

case class Punct(cats: TokenizerParse*) extends TokenizerParse

case class Char(cats: TokenizerParse*) extends TokenizerParse

case class OptWS(cats: TokenizerParse*) extends TokenizerParse

case class S(cats: TokenizerParse*) extends TokenizerParse

// A ParseResult is a pair consisting of a parse tree of what has been recognized so far, and an offset that
// indicates how far along in the input sequence we are.
//sealed case class ParseResult(success: Boolean)
//
//case object ParseFailure extends ParseResult(false)
//
//case class ParseSuccess(pos: Int, parse: Parse) extends ParseResult(true)


object Tokenizer extends Parser[TokenizerParse] {

  val punctuation = ".?!,\"'".toCharArray.map(_.toInt).sortWith(_ < _)
  val whitespace = " \n\t".toCharArray.map(_.toInt).sortWith(_ < _)

  override def parseLongest(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    parseS(from, to, input)
  }
  
  override def codeForExternalSymbol(name: String): Int = 0
  
  def parseS(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res1 = parsePlus1(from, to, input)
    res1 match {
      case ParseFailure => res1
      case ParseSuccess(next1, parse1) =>
        val res2 = parseOptWS(next1, to, input)
        res2 match {
          case ParseFailure => res2
          case ParseSuccess(next2, parse2) => ParseSuccess(next2, S(parse1, parse2))
        }
    }
  }
  
  def parsePlus1(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res1 = parseWordOrPunct(from, to, input)
    res1 match {
      case ParseFailure => res1
      case ParseSuccess(next, parse) =>
        val dtrs = mutable.Buffer(parse)
        var pos = next
        while (pos < to) {
          val res2 = parseWordOrPunct(pos, to, input)
          res2 match {
            case ParseFailure => return ParseSuccess(pos, Word(dtrs: _*))
            case ParseSuccess(next, parse) =>
              dtrs += parse
              pos = next
          }
        }
        ParseSuccess(pos, Word(dtrs: _*))
    }
  }
  
  def parseWordOrPunct(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res1 = parseOptWS(from, to, input)
    res1 match {
      case ParseFailure => res1
      case ParseSuccess(next1, parse1) =>
        val res2 = parseAlt1(next1, to, input)
        res2 match {
          case ParseFailure => res2
          case ParseSuccess(next2, parse2) => ParseSuccess(next2, WordOrPunct(parse1, parse2))
        }
    }
  }

  def parseAlt1(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res1 = parsePunct(from, to, input)
    if (res1.success) res1
    else parseWord(from, to, input)
  }
  
  def parseWord(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res1 = parseChar(from, to, input)
    res1 match {
      case ParseFailure => res1
      case ParseSuccess(next, parse) =>
        val dtrs = mutable.Buffer(parse)
        var pos = next
        while (pos < to) {
          val res2 = parseChar(pos, to, input)
          res2 match {
            case ParseFailure => return ParseSuccess(pos, Word(dtrs: _*))
            case ParseSuccess(next, parse) =>
              dtrs += parse
              pos = next
          }
        }
        ParseSuccess(pos, Word(dtrs: _*))
    }
  }
  
  @inline
  def parseOptWS(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    var pos = from
    val dtrs = mutable.Buffer[TokenizerParse]()
    while (pos < to) {
      val res = parseWsChars(pos, to, input)
      res match {
        case ParseSuccess(newPos, parse) =>
          pos = newPos
          dtrs += parse
        case ParseFailure => return ParseSuccess(pos, OptWS(dtrs: _*))
      } 
    }
    ParseSuccess(pos, OptWS(dtrs: _*))
  }

  @inline
  def parseWsChars(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    if (from >= to) ParseFailure
    else if (java.util.Arrays.binarySearch(whitespace, input(from)) >= 0) ParseSuccess(from + 1, Position(from))
    else ParseFailure
  }

  @inline
  def parsePunct(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    if (from >= to) ParseFailure
    else if (java.util.Arrays.binarySearch(punctuation, input(from)) >= 0) ParseSuccess(from + 1, Punct(Position(from)))
    else ParseFailure
  }

  @inline
  def parseChar(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res1 = parseRange1(from, to, input)
    if (res1.success) res1
    else parseRange2(from, to, input)
  }
  
  @inline
  def parseRange1(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    if (from >= to) ParseFailure
    else {
      val c = input(from)
      if (c >= 'a' && c <= 'z') ParseSuccess(from + 1, Position(from))
      else ParseFailure
    }
  }

  @inline
  def parseRange2(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    if (from >= to) ParseFailure
    else {
      val c = input(from)
      if (c >= 'A' && c <= 'Z') ParseSuccess(from + 1, Position(from))
      else ParseFailure
    }
  }

}
