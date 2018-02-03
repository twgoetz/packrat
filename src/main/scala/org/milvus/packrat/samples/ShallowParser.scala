package org.milvus.packrat.samples

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

sealed trait ShallowParse

object ShallowParser extends Parser[ShallowParse] {
  
  case class S(dtrs: ShallowParse*) extends ShallowParse
  case class CHUNK(dtrs: ShallowParse*) extends ShallowParse
  case class NP(dtrs: ShallowParse*) extends ShallowParse
  case class AP(dtrs: ShallowParse*) extends ShallowParse
  case class NBAR(dtrs: ShallowParse*) extends ShallowParse
  case class NOUNS(dtrs: ShallowParse*) extends ShallowParse
  case class PP(dtrs: ShallowParse*) extends ShallowParse
  case class IMPERATIVE(dtrs: ShallowParse*) extends ShallowParse
  case class Position(pos: Int) extends ShallowParse
  
  case class Result(success: Boolean, pos: Int, cats: mutable.Buffer[ShallowParse])
  val failure = Result(false, 0, mutable.Buffer())
  
  sealed abstract class ExternalSymbol(val name: String, val index: Int)
  case object UnknownSymbol extends ExternalSymbol("", -1)
  case object QUESTION extends ExternalSymbol("QUESTION", -2)
  case object PRP extends ExternalSymbol("PRP", -3)
  case object PDT extends ExternalSymbol("PDT", -4)
  case object DT extends ExternalSymbol("DT", -5)
  case object RBR extends ExternalSymbol("RBR", -6)
  case object ADJ extends ExternalSymbol("ADJ", -7)
  case object CD extends ExternalSymbol("CD", -8)
  case object NN extends ExternalSymbol("NN", -9)
  case object NNS extends ExternalSymbol("NNS", -10)
  case object NNP extends ExternalSymbol("NNP", -11)
  case object NNPS extends ExternalSymbol("NNPS", -12)
  case object P extends ExternalSymbol("P", -13)
  case object VB extends ExternalSymbol("VB", -14)
  
  private val externalSymbolMap: Map[String, ExternalSymbol] =
    Seq(QUESTION, PRP, PDT, DT, RBR, ADJ, CD, NN, NNS, NNP, NNPS, P, VB).foldLeft(
      Map[String, ExternalSymbol]())((map, sym) => map + (sym.name -> sym))
  
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
      val res = parseIMPERATIVE(from, to, input)
      if (res.success) res
      else {
        val res = {
          val ch = input(from)
          if (ch == -2) Result(true, from + 1, mutable.Buffer[ShallowParse](Position(from)))
          else failure
        }
        if (res.success) res
        else {
          val res = parseCHUNK(from, to, input)
          if (!res.success) {
            res
          } else {
            val dtrs = res.cats
            var pos0 = res.pos
            var keepGoing = true
            while (keepGoing && pos0 < to) {
              val res = parseCHUNK(pos0, to, input)
              if (!res.success) {
                keepGoing = false
              } else {
                dtrs ++= res.cats
                pos0 = res.pos
              }
            }
            Result(true, pos0, dtrs)
          }
        }
      }
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](S(res.cats: _*)))
    else failure
  }
  
  def parseCHUNK(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = parsePP(from, to, input)
      if (res.success) res
      else parseNP(from, to, input)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](CHUNK(res.cats: _*)))
    else failure
  }
  
  def parseNP(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val ch = input(from)
        if (ch == -3) Result(true, from + 1, mutable.Buffer[ShallowParse](Position(from)))
        else failure
      }
      if (res.success) res
      else {
        val dtrs = mutable.Buffer[ShallowParse]()
        val pos1_0 = from
        val res1 = {
          val res = {
            val ch = input(pos1_0)
            if (ch == -4) Result(true, pos1_0 + 1, mutable.Buffer[ShallowParse](Position(pos1_0)))
            else failure
          }
          if (res.success) res else Result(true, pos1_0, mutable.Buffer[ShallowParse]())
        }
        if (res1.success && res1.pos < input.size) {
          dtrs ++= res1.cats
          val pos1_1 = res1.pos
          val res2 = {
            val res = {
              val ch = input(pos1_1)
              if (ch == -5) Result(true, pos1_1 + 1, mutable.Buffer[ShallowParse](Position(pos1_1)))
              else failure
            }
            if (res.success) res else Result(true, pos1_1, mutable.Buffer[ShallowParse]())
          }
          if (res2.success && res2.pos < input.size) {
            dtrs ++= res2.cats
            val pos1_2 = res2.pos
            
            val res3 = parseNBAR(pos1_2, to, input)
            if (res3.success) {
              dtrs ++= res3.cats
              Result(true, res3.pos, dtrs)
            } else {
              failure
            }
          } else failure
        } else failure
      }
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](NP(res.cats: _*)))
    else failure
  }
  
  def parseAP(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos2_0 = from
      val res1 = {
        val res = {
          val ch = input(pos2_0)
          if (ch == -6) Result(true, pos2_0 + 1, mutable.Buffer[ShallowParse](Position(pos2_0)))
          else failure
        }
        if (res.success) res else Result(true, pos2_0, mutable.Buffer[ShallowParse]())
      }
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos2_1 = res1.pos
        val res2 = {
          val res = {
            val ch = input(pos2_1)
            if (ch == -7) Result(true, pos2_1 + 1, mutable.Buffer[ShallowParse](Position(pos2_1)))
            else failure
          }
          if (res.success) res else Result(true, pos2_1, mutable.Buffer[ShallowParse]())
        }
        if (res2.success && res2.pos < input.size) {
          dtrs ++= res2.cats
          val pos2_2 = res2.pos
          val res3 = {
            val res = {
              val ch = input(pos2_2)
              if (ch == -8) Result(true, pos2_2 + 1, mutable.Buffer[ShallowParse](Position(pos2_2)))
              else failure
            }
            if (res.success) res else Result(true, pos2_2, mutable.Buffer[ShallowParse]())
          }
          if (res3.success && res3.pos < input.size) {
            dtrs ++= res3.cats
            val pos2_3 = res3.pos
            
            val res4 = {
              val dtrs = mutable.Buffer[ShallowParse]()
              var pos3 = pos2_3
              var keepGoing = true
              while (keepGoing && pos3 < to) {
                val res = {
                  val ch = input(pos3)
                  if (ch == -7) Result(true, pos3 + 1, mutable.Buffer[ShallowParse](Position(pos3)))
                  else failure
                }
                if (!res.success) {
                  keepGoing = false
                } else {
                  dtrs ++= res.cats
                  pos3 = res.pos
                }
              }
              Result(true, pos3, dtrs)
            }
            if (res4.success) {
              dtrs ++= res4.cats
              Result(true, res4.pos, dtrs)
            } else {
              failure
            }
          } else failure
        } else failure
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](AP(res.cats: _*)))
    else failure
  }
  
  def parseNBAR(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos4_0 = from
      val res1 = {
        val res = parseAP(pos4_0, to, input)
        if (res.success) res else Result(true, pos4_0, mutable.Buffer[ShallowParse]())
      }
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos4_1 = res1.pos
        
        val res2 = parseNOUNS(pos4_1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](NBAR(res.cats: _*)))
    else failure
  }
  
  def parseNOUNS(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val res = {
          val ch = input(from)
          if (ch == -9) Result(true, from + 1, mutable.Buffer[ShallowParse](Position(from)))
          else failure
        }
        if (res.success) res
        else {
          val res = {
            val ch = input(from)
            if (ch == -10) Result(true, from + 1, mutable.Buffer[ShallowParse](Position(from)))
            else failure
          }
          if (res.success) res
          else {
            val res = {
              val ch = input(from)
              if (ch == -11) Result(true, from + 1, mutable.Buffer[ShallowParse](Position(from)))
              else failure
            }
            if (res.success) res
            else {
              val ch = input(from)
              if (ch == -12) Result(true, from + 1, mutable.Buffer[ShallowParse](Position(from)))
              else failure
            }
          }
        }
      }
      if (!res.success) {
        res
      } else {
        val dtrs = res.cats
        var pos5 = res.pos
        var keepGoing = true
        while (keepGoing && pos5 < to) {
          val res = {
            val res = {
              val ch = input(pos5)
              if (ch == -9) Result(true, pos5 + 1, mutable.Buffer[ShallowParse](Position(pos5)))
              else failure
            }
            if (res.success) res
            else {
              val res = {
                val ch = input(pos5)
                if (ch == -10) Result(true, pos5 + 1, mutable.Buffer[ShallowParse](Position(pos5)))
                else failure
              }
              if (res.success) res
              else {
                val res = {
                  val ch = input(pos5)
                  if (ch == -11)
                    Result(true, pos5 + 1, mutable.Buffer[ShallowParse](Position(pos5)))
                  else failure
                }
                if (res.success) res
                else {
                  val ch = input(pos5)
                  if (ch == -12)
                    Result(true, pos5 + 1, mutable.Buffer[ShallowParse](Position(pos5)))
                  else failure
                }
              }
            }
          }
          if (!res.success) {
            keepGoing = false
          } else {
            dtrs ++= res.cats
            pos5 = res.pos
          }
        }
        Result(true, pos5, dtrs)
      }
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](NOUNS(res.cats: _*)))
    else failure
  }
  
  def parsePP(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos6_0 = from
      val res1 = {
        val ch = input(pos6_0)
        if (ch == -13) Result(true, pos6_0 + 1, mutable.Buffer[ShallowParse](Position(pos6_0)))
        else failure
      }
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos6_1 = res1.pos
        
        val res2 = parseNP(pos6_1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](PP(res.cats: _*)))
    else failure
  }
  
  def parseIMPERATIVE(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos7_0 = from
      val res1 = {
        val ch = input(pos7_0)
        if (ch == -14) Result(true, pos7_0 + 1, mutable.Buffer[ShallowParse](Position(pos7_0)))
        else failure
      }
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos7_1 = res1.pos
        
        val res2 = {
          val res = parseCHUNK(pos7_1, to, input)
          if (!res.success) {
            res
          } else {
            val dtrs = res.cats
            var pos8 = res.pos
            var keepGoing = true
            while (keepGoing && pos8 < to) {
              val res = parseCHUNK(pos8, to, input)
              if (!res.success) {
                keepGoing = false
              } else {
                dtrs ++= res.cats
                pos8 = res.pos
              }
            }
            Result(true, pos8, dtrs)
          }
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[ShallowParse](IMPERATIVE(res.cats: _*)))
    else failure
  }
}
