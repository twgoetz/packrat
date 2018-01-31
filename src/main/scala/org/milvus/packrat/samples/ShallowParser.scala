package org.milvus.packrat.samples

import org.milvus.packrat.Parser
import scala.collection.mutable

sealed trait ShallowParse

object ShallowParser extends Parser[ShallowParse] {
  
  case class S(dtrs: ShallowParse*) extends ShallowParse
  case class Position(pos: Int) extends ShallowParse
  
  case class Result(success: Boolean, pos: Int, cats: mutable.Buffer[ShallowParse])
  val failure = Result(false, 0, mutable.Buffer())
  
  sealed abstract class ExternalSymbol(val name: String, val index: Int)
  case object UnknownSymbol extends ExternalSymbol("", -1)
  case object NN extends ExternalSymbol("NN", -2)
  
  private val externalSymbolMap: Map[String, ExternalSymbol] =
    Seq(NN).foldLeft(Map[String, ExternalSymbol]())((map, sym) => map + (sym.name -> sym))
  
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
      val dtrs = mutable.Buffer[ShallowParse]()
      var pos0 = from
      var keepGoing = true
      while (keepGoing && pos0 < to) {
        val res = {
          var pos1 = pos0
          var res = {
            val ch = input(pos1)
            if (ch == -2) Result(true, pos1 + 1, mutable.Buffer[ShallowParse](Position(pos1)))
            else failure
          }
          while (!res.success && pos1 < to) {
            pos1 = pos1 + 1
            res = {
              val ch = input(pos1)
              if (ch == -2) Result(true, pos1 + 1, mutable.Buffer[ShallowParse](Position(pos1)))
              else failure
            }
          }
          res
        }
        if (!res.success) {
          keepGoing = false
        } else {
          dtrs ++= res.cats
          pos0 = res.pos
        }
      }
      Result(true, pos0, dtrs)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](S(res.cats: _*)))
    else failure
  }
}
