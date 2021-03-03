package org.milvus.packrat

//import org.milvus.packrat.Parser
import com.sun.org.apache.xalan.internal.xsltc.compiler.sym

import scala.collection.mutable

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
  
  def parseTokens(input: Seq[String]): Option[ParseTree] = {
    parse(input.map(codeForExternalSymbol))
  }
  
}

sealed trait TokenizerParse

object Tokenizer extends Parser[TokenizerParse] {
  
  case class S(dtrs: TokenizerParse*) extends TokenizerParse
  case class OptWS(dtrs: TokenizerParse*) extends TokenizerParse
  case class WordOrPunct(dtrs: TokenizerParse*) extends TokenizerParse
  case class Word(dtrs: TokenizerParse*) extends TokenizerParse
  case class Punct(dtrs: TokenizerParse*) extends TokenizerParse
  case class Position(pos: Int) extends TokenizerParse
  
  case class Result(success: Boolean, pos: Int, cats: mutable.Buffer[TokenizerParse])
  val failure = Result(false, 0, mutable.Buffer())
  
  sealed abstract class ExternalSymbol(val name: String, val index: Int)
  case object UnknownSymbol extends ExternalSymbol("", -1)
  case object Stuff extends ExternalSymbol("Stuff", -2)
  
  private val externalSymbolMap: Map[String, ExternalSymbol] =
    Seq(Stuff).foldLeft(Map[String, ExternalSymbol]())((map, sym) => map + (sym.name -> sym))
  
  override def codeForExternalSymbol(sym: String) =
    externalSymbolMap
      .getOrElse(sym, UnknownSymbol)
      .index
  
  override def parseLongest(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res = parseS(from, to, input)
    if (res.success) ParseSuccess(res.pos, res.cats(0))
    else ParseFailure
  }
  
  def parseS(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[TokenizerParse]()
      val next0 = from
      val res1 = {
        val res = parseWordOrPunct(next0, to, input)
        if (!res.success) {
          res
        } else {
          val dtrs = res.cats
          var pos = res.pos
          var keepGoing = true
          while (keepGoing && pos < to) {
            val res = parseWordOrPunct(pos, to, input)
            if (!res.success) {
              keepGoing = false
            } else {
              dtrs ++= res.cats
              pos = res.pos
            }
          }
          Result(true, pos, dtrs)
        }
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        
        val res2 = parseOptWS(next1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](S(res.cats.toSeq: _*)))
    else failure
  }
  
  def parseOptWS(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[TokenizerParse]()
      var pos = from
      var keepGoing = true
      while (keepGoing && pos < to) {
        val res = {
          val ch = input(pos)
          if (ch < 10) {
            if (ch == 9) Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos)))
            else failure
          } else if (ch > 10) {
            if (ch == 32) Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos)))
            else failure
          } else if (ch == 10) Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos)))
          else failure
        }
        if (!res.success) {
          keepGoing = false
        } else {
          dtrs ++= res.cats
          pos = res.pos
        }
      }
      Result(true, pos, dtrs)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](OptWS(res.cats.toSeq: _*)))
    else failure
  }
  
  def parseWordOrPunct(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[TokenizerParse]()
      val next0 = from
      val res1 = parseOptWS(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        
        val res2 = {
          val res = parsePunct(next1, to, input)
          if (res.success) res
          else parseWord(next1, to, input)
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success)
      Result(true, res.pos, mutable.Buffer[TokenizerParse](WordOrPunct(res.cats.toSeq: _*)))
    else failure
  }
  
  def parseWord(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val res =
          if (from >= input.size) failure
          else {
            val c = input(from)
            if (c >= 97 && c <= 122)
              Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
            else
              failure
          }
        if (res.success) res
        else if (from >= input.size) failure
        else {
          val c = input(from)
          if (c >= 65 && c <= 90)
            Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
          else
            failure
        }
      }
      if (!res.success) {
        res
      } else {
        val dtrs = res.cats
        var pos = res.pos
        var keepGoing = true
        while (keepGoing && pos < to) {
          val res = {
            val res =
              if (pos >= input.size) failure
              else {
                val c = input(pos)
                if (c >= 97 && c <= 122)
                  Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos)))
                else
                  failure
              }
            if (res.success) res
            else if (pos >= input.size) failure
            else {
              val c = input(pos)
              if (c >= 65 && c <= 90)
                Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos)))
              else
                failure
            }
          }
          if (!res.success) {
            keepGoing = false
          } else {
            dtrs ++= res.cats
            pos = res.pos
          }
        }
        Result(true, pos, dtrs)
      }
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](Word(res.cats.toSeq: _*)))
    else failure
  }
  
  def parsePunct(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val ch = input(from)
        if (ch < 44) {
          if (ch < 34) {
            if (ch == 33) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
            else failure
          } else if (ch > 34) {
            if (ch == 39) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
            else failure
          } else if (ch == 34)
                   Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
          else failure
        } else if (ch > 44) {
          if (ch == 46) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
          else if (ch == 63) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
          else failure
        } else if (ch == 44) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
        else failure
      }
      if (res.success) res
      else {
        val ch = input(from)
        if (ch == -2) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
        else failure
      }
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](Punct(res.cats.toSeq: _*)))
    else failure
  }
}

