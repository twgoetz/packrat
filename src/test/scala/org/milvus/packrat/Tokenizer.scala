package org.milvus.packrat.test

import org.milvus.packrat.Parser
import scala.collection.mutable

sealed trait TokenizerParse

case class S(dtrs: TokenizerParse*) extends TokenizerParse
case class OptWS(dtrs: TokenizerParse*) extends TokenizerParse
case class WordOrPunct(dtrs: TokenizerParse*) extends TokenizerParse
case class Word(dtrs: TokenizerParse*) extends TokenizerParse
case class Char(dtrs: TokenizerParse*) extends TokenizerParse
case class Punct(dtrs: TokenizerParse*) extends TokenizerParse
case class Position(pos: Int) extends TokenizerParse

case class Result(success: Boolean, pos: Int, cats: mutable.Buffer[TokenizerParse])

object Tokenizer extends Parser[TokenizerParse] {

  val failure = Result(false, 0, mutable.Buffer())

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
          while (pos < to) {
            val res = parseWordOrPunct(pos, to, input)
            if (!res.success) {
              pos = to // break
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
      }
      else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](S(res.cats: _*))) else failure
  }

  def parseOptWS(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[TokenizerParse]()
      var pos = from
      while (pos < to) {
        val res = {
          val ch = input(pos)
          if (ch < 92) {
            if (ch == 32) Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos))) else failure
          }
          else if (ch > 92) {
            if (ch == 92) Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos))) else failure
          }
          else if (ch == 92) Result(true, pos + 1, mutable.Buffer[TokenizerParse](Position(pos)))
          else failure
        }
        if (!res.success) {
          pos = to
        }
        else {
          dtrs ++= res.cats
          pos = res.pos
        }
      }
      Result(true, pos, dtrs)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](OptWS(res.cats: _*))) else failure
  }

  def parseWordOrPunct(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = parsePunct(from, to, input)
      if (res.success) res
      else parseWord(from, to, input)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](WordOrPunct(res.cats: _*))) else failure
  }

  def parseWord(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = parseChar(from, to, input)
      if (!res.success) {
        res
      } else {
        val dtrs = res.cats
        var pos = res.pos
        while (pos < to) {
          val res = parseChar(pos, to, input)
          if (!res.success) {
            pos = to // break
          } else {
            dtrs ++= res.cats
            pos = res.pos
          }
        }
        Result(true, pos, dtrs)
      }
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](Word(res.cats: _*))) else failure
  }

  def parseChar(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = if (to >= input.size) failure
      else {
        val c = input(from)
        if (c >= 97 && c <= 122)
          Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
        else
          failure}
      if (res.success) res
      else if (to >= input.size) failure
      else {
        val c = input(from)
        if (c >= 65 && c <= 90)
          Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
        else
          failure}
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](Char(res.cats: _*))) else failure
  }

  def parsePunct(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val ch = input(from)
      if (ch < 44) {
        if (ch < 34) {
          if (ch == 33) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from))) else failure
        }
        else if (ch > 34) {
          if (ch == 39) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from))) else failure
        }
        else if (ch == 34) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
        else failure
      }
      else if (ch > 44) {
        if (ch == 46) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
        else if(ch == 63) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
        else failure
      }
      else if (ch == 44) Result(true, from + 1, mutable.Buffer[TokenizerParse](Position(from)))
      else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[TokenizerParse](Punct(res.cats: _*))) else failure
  }
}
