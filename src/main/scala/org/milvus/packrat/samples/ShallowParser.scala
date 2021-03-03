package org.milvus.packrat.samples

import scala.collection.mutable

// This file was generated automatically. Do not edit.

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

  abstract class NonTerminal(val name: String, val dtrs: Seq[ShallowParse]) extends ShallowParse
  object NonTerminal { def unapply(nt: NonTerminal): Option[(String, Seq[ShallowParse])] = Some((nt.name, nt.dtrs)) }
  case class S(override val dtrs: ShallowParse*) extends NonTerminal("S", dtrs)
  case class CHUNK(override val dtrs: ShallowParse*) extends NonTerminal("CHUNK", dtrs)
  case class NP(override val dtrs: ShallowParse*) extends NonTerminal("NP", dtrs)
  case class AP(override val dtrs: ShallowParse*) extends NonTerminal("AP", dtrs)
  case class ADJ(override val dtrs: ShallowParse*) extends NonTerminal("ADJ", dtrs)
  case class NBAR(override val dtrs: ShallowParse*) extends NonTerminal("NBAR", dtrs)
  case class NOUNS(override val dtrs: ShallowParse*) extends NonTerminal("NOUNS", dtrs)
  case class PP(override val dtrs: ShallowParse*) extends NonTerminal("PP", dtrs)
  case class IMPERATIVE(override val dtrs: ShallowParse*) extends NonTerminal("IMPERATIVE", dtrs)
  case class QUESTION(override val dtrs: ShallowParse*) extends NonTerminal("QUESTION", dtrs)
  case class VERB(override val dtrs: ShallowParse*) extends NonTerminal("VERB", dtrs)
  case class Position(pos: Int) extends ShallowParse

  sealed abstract class Result(val success: Boolean, val pos: Int, val cats: mutable.Buffer[ShallowParse])
  case class Success(override val pos: Int, override val cats: mutable.Buffer[ShallowParse]) extends Result(true, pos, cats)
  case object Failure extends Result(false, 0, mutable.Buffer())

  sealed abstract class ExternalSymbol(val name: String, val index: Int)
  case object UnknownSymbol extends ExternalSymbol("", -1)
  case object PRP extends ExternalSymbol("PRP", -2)
  case object WDT extends ExternalSymbol("WDT", -3)
  case object PDT extends ExternalSymbol("PDT", -4)
  case object DT extends ExternalSymbol("DT", -5)
  case object RBR extends ExternalSymbol("RBR", -6)
  case object CD extends ExternalSymbol("CD", -7)
  case object JJ extends ExternalSymbol("JJ", -8)
  case object JJS extends ExternalSymbol("JJS", -9)
  case object JJR extends ExternalSymbol("JJR", -10)
  case object NN extends ExternalSymbol("NN", -11)
  case object NNS extends ExternalSymbol("NNS", -12)
  case object NNP extends ExternalSymbol("NNP", -13)
  case object NNPS extends ExternalSymbol("NNPS", -14)
  case object IN extends ExternalSymbol("IN", -15)
  case object VB extends ExternalSymbol("VB", -16)
  case object VBD extends ExternalSymbol("VBD", -17)
  case object VBG extends ExternalSymbol("VBG", -18)
  case object VBN extends ExternalSymbol("VBN", -19)
  case object VBP extends ExternalSymbol("VBP", -20)
  case object VBZ extends ExternalSymbol("VBZ", -21)

  private val externalSymbolMap: Map[String, ExternalSymbol] = Seq(PRP, WDT, PDT, DT, RBR, CD, JJ, JJS, JJR, NN, NNS, NNP, NNPS, IN, VB, VBD, VBG, VBN, VBP, VBZ).foldLeft(Map[String, ExternalSymbol]())((map, sym) => map + (sym.name -> sym))

  override def codeForExternalSymbol(sym: String) = externalSymbolMap
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
        val res = parseQUESTION(from, to, input)
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
            Success(pos0, dtrs)
          }
        }
      }
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](S(res.cats.toSeq: _*))) else Failure
  }

  def parseCHUNK(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = parsePP(from, to, input)
      if (res.success) res
      else parseNP(from, to, input)
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](CHUNK(res.cats.toSeq: _*))) else Failure
  }

  def parseNP(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val ch = input(from)
        if (ch == -2) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
      }
      if (res.success) res
      else {
        val dtrs = mutable.Buffer[ShallowParse]()
        val pos1_0 = from
        val res1 = {
          val res = {
            val res = {
              val ch = input(pos1_0)
              if (ch == -3) Success(pos1_0 + 1, mutable.Buffer[ShallowParse](Position(pos1_0))) else Failure
            }
            if (res.success) res
            else {
              val dtrs = mutable.Buffer[ShallowParse]()
              val pos2_0 = pos1_0
              val res1 = {
                val res = {
                  val ch = input(pos2_0)
                  if (ch == -4) Success(pos2_0 + 1, mutable.Buffer[ShallowParse](Position(pos2_0))) else Failure
                }
                if (res.success) res else Success(pos2_0, mutable.Buffer[ShallowParse]())
              }
              if (res1.success && res1.pos < input.size) {
                dtrs ++= res1.cats
                val pos2_1 = res1.pos

                val res2 = {
                  val ch = input(pos2_1)
                  if (ch == -5) Success(pos2_1 + 1, mutable.Buffer[ShallowParse](Position(pos2_1))) else Failure
                }
                if (res2.success) {
                  dtrs ++= res2.cats
                  Success(res2.pos, dtrs)
                } else {
                  Failure
                }
              }
              else Failure
            }
          }
          if (res.success) res else Success(pos1_0, mutable.Buffer[ShallowParse]())
        }
        if (res1.success && res1.pos < input.size) {
          dtrs ++= res1.cats
          val pos1_1 = res1.pos

          val res2 = parseNBAR(pos1_1, to, input)
          if (res2.success) {
            dtrs ++= res2.cats
            Success(res2.pos, dtrs)
          } else {
            Failure
          }
        }
        else Failure
      }
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](NP(res.cats.toSeq: _*))) else Failure
  }

  def parseAP(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos3_0 = from
      val res1 = {
        val res = {
          val ch = input(pos3_0)
          if (ch == -6) Success(pos3_0 + 1, mutable.Buffer[ShallowParse](Position(pos3_0))) else Failure
        }
        if (res.success) res else Success(pos3_0, mutable.Buffer[ShallowParse]())
      }
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos3_1 = res1.pos
        val res2 = {
          val res = parseADJ(pos3_1, to, input)
          if (res.success) res else Success(pos3_1, mutable.Buffer[ShallowParse]())
        }
        if (res2.success && res2.pos < input.size) {
          dtrs ++= res2.cats
          val pos3_2 = res2.pos
          val res3 = {
            val res = {
              val ch = input(pos3_2)
              if (ch == -7) Success(pos3_2 + 1, mutable.Buffer[ShallowParse](Position(pos3_2))) else Failure
            }
            if (res.success) res else Success(pos3_2, mutable.Buffer[ShallowParse]())
          }
          if (res3.success && res3.pos < input.size) {
            dtrs ++= res3.cats
            val pos3_3 = res3.pos

            val res4 = {
              val dtrs = mutable.Buffer[ShallowParse]()
              var pos4 = pos3_3
              var keepGoing = true
              while (keepGoing && pos4 < to) {
                val res = parseADJ(pos4, to, input)
                if (!res.success) {
                  keepGoing = false
                }
                else {
                  dtrs ++= res.cats
                  pos4 = res.pos
                }
              }
              Success(pos4, dtrs)
            }
            if (res4.success) {
              dtrs ++= res4.cats
              Success(res4.pos, dtrs)
            } else {
              Failure
            }
          }
          else Failure
        }
        else Failure
      }
      else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](AP(res.cats.toSeq: _*))) else Failure
  }

  def parseADJ(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val ch = input(from)
        if (ch == -8) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
      }
      if (res.success) res
      else {
        val res = {
          val ch = input(from)
          if (ch == -9) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
        }
        if (res.success) res
        else {
          val ch = input(from)
          if (ch == -10) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
        }
      }
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](ADJ(res.cats.toSeq: _*))) else Failure
  }

  def parseNBAR(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos5_0 = from
      val res1 = {
        val res = parseAP(pos5_0, to, input)
        if (res.success) res else Success(pos5_0, mutable.Buffer[ShallowParse]())
      }
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos5_1 = res1.pos

        val res2 = parseNOUNS(pos5_1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      }
      else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](NBAR(res.cats.toSeq: _*))) else Failure
  }

  def parseNOUNS(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val res = {
          val ch = input(from)
          if (ch == -11) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
        }
        if (res.success) res
        else {
          val res = {
            val ch = input(from)
            if (ch == -12) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
          }
          if (res.success) res
          else {
            val res = {
              val ch = input(from)
              if (ch == -13) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
            }
            if (res.success) res
            else {
              val ch = input(from)
              if (ch == -14) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
            }
          }
        }
      }
      if (!res.success) {
        res
      } else {
        val dtrs = res.cats
        var pos6 = res.pos
        var keepGoing = true
        while (keepGoing && pos6 < to) {
          val res = {
            val res = {
              val ch = input(pos6)
              if (ch == -11) Success(pos6 + 1, mutable.Buffer[ShallowParse](Position(pos6))) else Failure
            }
            if (res.success) res
            else {
              val res = {
                val ch = input(pos6)
                if (ch == -12) Success(pos6 + 1, mutable.Buffer[ShallowParse](Position(pos6))) else Failure
              }
              if (res.success) res
              else {
                val res = {
                  val ch = input(pos6)
                  if (ch == -13) Success(pos6 + 1, mutable.Buffer[ShallowParse](Position(pos6))) else Failure
                }
                if (res.success) res
                else {
                  val ch = input(pos6)
                  if (ch == -14) Success(pos6 + 1, mutable.Buffer[ShallowParse](Position(pos6))) else Failure
                }
              }
            }
          }
          if (!res.success) {
            keepGoing = false
          } else {
            dtrs ++= res.cats
            pos6 = res.pos
          }
        }
        Success(pos6, dtrs)
      }
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](NOUNS(res.cats.toSeq: _*))) else Failure
  }

  def parsePP(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos7_0 = from
      val res1 = {
        val ch = input(pos7_0)
        if (ch == -15) Success(pos7_0 + 1, mutable.Buffer[ShallowParse](Position(pos7_0))) else Failure
      }
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos7_1 = res1.pos

        val res2 = parseNP(pos7_1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      }
      else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](PP(res.cats.toSeq: _*))) else Failure
  }

  def parseIMPERATIVE(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos8_0 = from
      val res1 = parseVERB(pos8_0, to, input)
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos8_1 = res1.pos

        val res2 = {
          val res = {
            var pos10 = pos8_1
            var res = parseCHUNK(pos10, to, input)
            while (!res.success && (pos10 + 1) < to) {
              pos10 = pos10 + 1
              res = parseCHUNK(pos10, to, input)
            }
            res
          }
          if (!res.success) {
            res
          } else {
            val dtrs = res.cats
            var pos9 = res.pos
            var keepGoing = true
            while (keepGoing && pos9 < to) {
              val res = {
                var pos11 = pos9
                var res = parseCHUNK(pos11, to, input)
                while (!res.success && (pos11 + 1) < to) {
                  pos11 = pos11 + 1
                  res = parseCHUNK(pos11, to, input)
                }
                res
              }
              if (!res.success) {
                keepGoing = false
              } else {
                dtrs ++= res.cats
                pos9 = res.pos
              }
            }
            Success(pos9, dtrs)
          }
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      }
      else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](IMPERATIVE(res.cats.toSeq: _*))) else Failure
  }

  def parseQUESTION(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[ShallowParse]()
      val pos12_0 = from
      val res1 = parseCHUNK(pos12_0, to, input)
      if (res1.success && res1.pos < input.size) {
        dtrs ++= res1.cats
        val pos12_1 = res1.pos
        val res2 = parseVERB(pos12_1, to, input)
        if (res2.success && res2.pos < input.size) {
          dtrs ++= res2.cats
          val pos12_2 = res2.pos
          val res3 = {
            val dtrs = mutable.Buffer[ShallowParse]()
            var pos13 = pos12_2
            var keepGoing = true
            while (keepGoing && pos13 < to) {
              val res = {
                var pos14 = pos13
                var res = parseCHUNK(pos14, to, input)
                while (!res.success && (pos14 + 1) < to) {
                  pos14 = pos14 + 1
                  res = parseCHUNK(pos14, to, input)
                }
                res
              }
              if (!res.success) {
                keepGoing = false
              }
              else {
                dtrs ++= res.cats
                pos13 = res.pos
              }
            }
            Success(pos13, dtrs)
          }
          if (res3.success && res3.pos < input.size) {
            dtrs ++= res3.cats
            val pos12_3 = res3.pos
            val res4 = {
              val res = parseVERB(pos12_3, to, input)
              if (res.success) res else Success(pos12_3, mutable.Buffer[ShallowParse]())
            }
            if (res4.success && res4.pos < input.size) {
              dtrs ++= res4.cats
              val pos12_4 = res4.pos

              val res5 = {
                val dtrs = mutable.Buffer[ShallowParse]()
                var pos15 = pos12_4
                var keepGoing = true
                while (keepGoing && pos15 < to) {
                  val res = {
                    var pos16 = pos15
                    var res = parseCHUNK(pos16, to, input)
                    while (!res.success && (pos16 + 1) < to) {
                      pos16 = pos16 + 1
                      res = parseCHUNK(pos16, to, input)
                    }
                    res
                  }
                  if (!res.success) {
                    keepGoing = false
                  }
                  else {
                    dtrs ++= res.cats
                    pos15 = res.pos
                  }
                }
                Success(pos15, dtrs)
              }
              if (res5.success) {
                dtrs ++= res5.cats
                Success(res5.pos, dtrs)
              } else {
                Failure
              }
            }
            else Failure
          }
          else Failure
        }
        else Failure
      }
      else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](QUESTION(res.cats.toSeq: _*))) else Failure
  }

  def parseVERB(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val ch = input(from)
        if (ch == -16) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
      }
      if (res.success) res
      else {
        val res = {
          val ch = input(from)
          if (ch == -17) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
        }
        if (res.success) res
        else {
          val res = {
            val ch = input(from)
            if (ch == -18) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
          }
          if (res.success) res
          else {
            val res = {
              val ch = input(from)
              if (ch == -19) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
            }
            if (res.success) res
            else {
              val res = {
                val ch = input(from)
                if (ch == -20) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
              }
              if (res.success) res
              else {
                val ch = input(from)
                if (ch == -21) Success(from + 1, mutable.Buffer[ShallowParse](Position(from))) else Failure
              }
            }
          }
        }
      }
    }
    if (res.success) Success(res.pos, mutable.Buffer[ShallowParse](VERB(res.cats.toSeq: _*))) else Failure
  }
}


