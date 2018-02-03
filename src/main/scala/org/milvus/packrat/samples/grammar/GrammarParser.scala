package org.milvus.packrat.samples.grammar

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


sealed trait GrammarParse

object GrammarParser extends Parser[GrammarParse] {

  case class Grammar(dtrs: GrammarParse*) extends GrammarParse
  case class Rule(dtrs: GrammarParse*) extends GrammarParse
  case class Expr(dtrs: GrammarParse*) extends GrammarParse
  case class SeqElementExpr(dtrs: GrammarParse*) extends GrammarParse
  case class AltElementExpr(dtrs: GrammarParse*) extends GrammarParse
  case class OperandExpr(dtrs: GrammarParse*) extends GrammarParse
  case class ParenExpr(dtrs: GrammarParse*) extends GrammarParse
  case class SimpleExpr(dtrs: GrammarParse*) extends GrammarParse
  case class GapExpr(dtrs: GrammarParse*) extends GrammarParse
  case class Terminal(dtrs: GrammarParse*) extends GrammarParse
  case class Symbol(dtrs: GrammarParse*) extends GrammarParse
  case class CharSet(dtrs: GrammarParse*) extends GrammarParse
  case class EscapedChar(dtrs: GrammarParse*) extends GrammarParse
  case class Hex(dtrs: GrammarParse*) extends GrammarParse
  case class HexChar(dtrs: GrammarParse*) extends GrammarParse
  case class Range(dtrs: GrammarParse*) extends GrammarParse
  case class OptWS(dtrs: GrammarParse*) extends GrammarParse
  case class Operator(dtrs: GrammarParse*) extends GrammarParse
  case class GapOperator(dtrs: GrammarParse*) extends GrammarParse
  case class AltOperator(dtrs: GrammarParse*) extends GrammarParse
  case class Position(pos: Int) extends GrammarParse

  sealed class Result(val success: Boolean, val pos: Int, val cats: mutable.Buffer[GrammarParse])
  case class Success(override val pos: Int, override val cats: mutable.Buffer[GrammarParse])
      extends Result(true, pos, cats)
  case object Failure extends Result(false, 0, mutable.Buffer())

  override def parseLongest(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res = parseGrammar(from, to, input)
    if (res.success) ParseSuccess(res.pos, res.cats(0))
    else ParseFailure
  }
  
  override def codeForExternalSymbol(name: String): Int = 0

  def parseGrammar(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      var pos = from
      var keepGoing = true
      while (keepGoing && pos < to) {
        val res = parseRule(pos, to, input)
        if (!res.success) {
          keepGoing = false
        } else {
          dtrs ++= res.cats
          pos = res.pos
        }
      }
      Success(pos, dtrs)
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Grammar(res.cats: _*)))
    else Failure
  }

  def parseRule(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseOptWS(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = parseSymbol(next1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos
          val res3 = parseOptWS(next2, to, input)
          if (res3.success) {
            dtrs ++= res3.cats
            val next3 = res3.pos
            val res4 = {
              val ch = input(next3)
              if (ch == 45) Success(next3 + 1, mutable.Buffer[GrammarParse](Position(next3)))
              else Failure
            }
            if (res4.success) {
              dtrs ++= res4.cats
              val next4 = res4.pos
              val res5 = {
                val ch = input(next4)
                if (ch == 62) Success(next4 + 1, mutable.Buffer[GrammarParse](Position(next4)))
                else Failure
              }
              if (res5.success) {
                dtrs ++= res5.cats
                val next5 = res5.pos
                val res6 = parseOptWS(next5, to, input)
                if (res6.success) {
                  dtrs ++= res6.cats
                  val next6 = res6.pos
                  val res7 = parseExpr(next6, to, input)
                  if (res7.success) {
                    dtrs ++= res7.cats
                    val next7 = res7.pos
                    val res8 = parseOptWS(next7, to, input)
                    if (res8.success) {
                      dtrs ++= res8.cats
                      val next8 = res8.pos
                      val res9 = {
                        val ch = input(next8)
                        if (ch == 59)
                          Success(next8 + 1, mutable.Buffer[GrammarParse](Position(next8)))
                        else Failure
                      }
                      if (res9.success) {
                        dtrs ++= res9.cats
                        val next9 = res9.pos

                        val res10 = parseOptWS(next9, to, input)
                        if (res10.success) {
                          dtrs ++= res10.cats
                          Success(res10.pos, dtrs)
                        } else {
                          Failure
                        }
                      } else Failure
                    } else Failure
                  } else Failure
                } else Failure
              } else Failure
            } else Failure
          } else Failure
        } else Failure
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Rule(res.cats: _*))) else Failure
  }

  def parseExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseSeqElementExpr(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos

        val res2 = {
          val dtrs = mutable.Buffer[GrammarParse]()
          var pos = next1
          var keepGoing = true
          while (keepGoing && pos < to) {
            val res = {
              val dtrs = mutable.Buffer[GrammarParse]()
              val next0 = pos
              val res1 = parseOptWS(next0, to, input)
              if (res1.success) {
                dtrs ++= res1.cats
                val next1 = res1.pos

                val res2 = parseSeqElementExpr(next1, to, input)
                if (res2.success) {
                  dtrs ++= res2.cats
                  Success(res2.pos, dtrs)
                } else {
                  Failure
                }
              } else Failure
            }
            if (!res.success) {
              keepGoing = false
            } else {
              dtrs ++= res.cats
              pos = res.pos
            }
          }
          Success(pos, dtrs)
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Expr(res.cats: _*))) else Failure
  }

  def parseSeqElementExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseAltElementExpr(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos

        val res2 = {
          val dtrs = mutable.Buffer[GrammarParse]()
          var pos = next1
          var keepGoing = true
          while (keepGoing && pos < to) {
            val res = {
              val dtrs = mutable.Buffer[GrammarParse]()
              val next0 = pos
              val res1 = parseOptWS(next0, to, input)
              if (res1.success) {
                dtrs ++= res1.cats
                val next1 = res1.pos
                val res2 = parseAltOperator(next1, to, input)
                if (res2.success) {
                  dtrs ++= res2.cats
                  val next2 = res2.pos
                  val res3 = parseOptWS(next2, to, input)
                  if (res3.success) {
                    dtrs ++= res3.cats
                    val next3 = res3.pos

                    val res4 = parseAltElementExpr(next3, to, input)
                    if (res4.success) {
                      dtrs ++= res4.cats
                      Success(res4.pos, dtrs)
                    } else {
                      Failure
                    }
                  } else Failure
                } else Failure
              } else Failure
            }
            if (!res.success) {
              keepGoing = false
            } else {
              dtrs ++= res.cats
              pos = res.pos
            }
          }
          Success(pos, dtrs)
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](SeqElementExpr(res.cats: _*)))
    else Failure
  }

  def parseAltElementExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseOperandExpr(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos

        val res2 = {
          val res = {
            val dtrs = mutable.Buffer[GrammarParse]()
            val next0 = next1
            val res1 = parseOptWS(next0, to, input)
            if (res1.success) {
              dtrs ++= res1.cats
              val next1 = res1.pos

              val res2 = parseOperator(next1, to, input)
              if (res2.success) {
                dtrs ++= res2.cats
                Success(res2.pos, dtrs)
              } else {
                Failure
              }
            } else Failure
          }
          if (res.success) res else Success(next1, mutable.Buffer[GrammarParse]())
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](AltElementExpr(res.cats: _*)))
    else Failure
  }

  def parseOperandExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = parseParenExpr(from, to, input)
      if (res.success) res
      else parseSimpleExpr(from, to, input)
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](OperandExpr(res.cats: _*)))
    else Failure
  }

  def parseParenExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 40) Success(next0 + 1, mutable.Buffer[GrammarParse](Position(next0))) else Failure
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = parseOptWS(next1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos
          val res3 = parseExpr(next2, to, input)
          if (res3.success) {
            dtrs ++= res3.cats
            val next3 = res3.pos
            val res4 = parseOptWS(next3, to, input)
            if (res4.success) {
              dtrs ++= res4.cats
              val next4 = res4.pos

              val res5 = {
                val ch = input(next4)
                if (ch == 41) Success(next4 + 1, mutable.Buffer[GrammarParse](Position(next4)))
                else Failure
              }
              if (res5.success) {
                dtrs ++= res5.cats
                Success(res5.pos, dtrs)
              } else {
                Failure
              }
            } else Failure
          } else Failure
        } else Failure
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](ParenExpr(res.cats: _*)))
    else Failure
  }

  def parseSimpleExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = parseGapExpr(from, to, input)
      if (res.success) res
      else {
        val res = parseRange(from, to, input)
        if (res.success) res
        else {
          val res = parseTerminal(from, to, input)
          if (res.success) res
          else {
            val res = parseSymbol(from, to, input)
            if (res.success) res
            else parseCharSet(from, to, input)
          }
        }
      }
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](SimpleExpr(res.cats: _*)))
    else Failure
  }

  def parseGapExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseGapOperator(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = parseOptWS(next1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos

          val res3 = parseExpr(next2, to, input)
          if (res3.success) {
            dtrs ++= res3.cats
            Success(res3.pos, dtrs)
          } else {
            Failure
          }
        } else Failure
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](GapExpr(res.cats: _*)))
    else Failure
  }

  def parseTerminal(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 39) Success(next0 + 1, mutable.Buffer[GrammarParse](Position(next0))) else Failure
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = {
          val res = parseHexChar(next1, to, input)
          if (res.success) res
          else {
            val res = parseEscapedChar(next1, to, input)
            if (res.success) res
            else {
              val res =
                if (next1 >= input.size) Failure
                else {
                  val c = input(next1)
                  if (c >= 97 && c <= 122)
                    Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                  else
                    Failure
                }
              if (res.success) res
              else {
                val res =
                  if (next1 >= input.size) Failure
                  else {
                    val c = input(next1)
                    if (c >= 65 && c <= 90)
                      Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                    else
                      Failure
                  }
                if (res.success) res
                else {
                  val res =
                    if (next1 >= input.size) Failure
                    else {
                      val c = input(next1)
                      if (c >= 48 && c <= 57)
                        Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else
                        Failure
                    }
                  if (res.success) res
                  else {
                    val ch = input(next1)
                    if (ch < 47) {
                      if (ch < 42) {
                        if (ch < 40) {
                          if (ch == 33)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else if (ch == 35)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch > 40) {
                          if (ch == 41)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch == 40)
                          Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                        else Failure
                      } else if (ch > 42) {
                        if (ch < 45) {
                          if (ch == 43)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch > 45) {
                          if (ch == 46)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch == 45)
                          Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                        else Failure
                      } else if (ch == 42)
                        Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else Failure
                    } else if (ch > 47) {
                      if (ch < 91) {
                        if (ch < 62) {
                          if (ch == 59)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else if (ch == 60)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch > 62) {
                          if (ch == 63)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch == 62)
                          Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                        else Failure
                      } else if (ch > 91) {
                        if (ch < 94) {
                          if (ch == 93)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch > 94) {
                          if (ch == 95)
                            Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                          else Failure
                        } else if (ch == 94)
                          Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                        else Failure
                      } else if (ch == 91)
                        Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else Failure
                    } else if (ch == 47)
                      Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                    else Failure
                  }
                }
              }
            }
          }
        }
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos

          val res3 = {
            val ch = input(next2)
            if (ch == 39) Success(next2 + 1, mutable.Buffer[GrammarParse](Position(next2)))
            else Failure
          }
          if (res3.success) {
            dtrs ++= res3.cats
            Success(res3.pos, dtrs)
          } else {
            Failure
          }
        } else Failure
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Terminal(res.cats: _*)))
    else Failure
  }

  def parseSymbol(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val res =
          if (next0 >= input.size) Failure
          else {
            val c = input(next0)
            if (c >= 97 && c <= 122)
              Success(next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
            else
              Failure
          }
        if (res.success) res
        else if (next0 >= input.size) Failure
        else {
          val c = input(next0)
          if (c >= 65 && c <= 90)
            Success(next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
          else
            Failure
        }
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos

        val res2 = {
          val dtrs = mutable.Buffer[GrammarParse]()
          var pos = next1
          var keepGoing = true
          while (keepGoing && pos < to) {
            val res = {
              val res =
                if (pos >= input.size) Failure
                else {
                  val c = input(pos)
                  if (c >= 97 && c <= 122)
                    Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                  else
                    Failure
                }
              if (res.success) res
              else {
                val res =
                  if (pos >= input.size) Failure
                  else {
                    val c = input(pos)
                    if (c >= 65 && c <= 90)
                      Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                    else
                      Failure
                  }
                if (res.success) res
                else if (pos >= input.size) Failure
                else {
                  val c = input(pos)
                  if (c >= 48 && c <= 57)
                    Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                  else
                    Failure
                }
              }
            }
            if (!res.success) {
              keepGoing = false
            } else {
              dtrs ++= res.cats
              pos = res.pos
            }
          }
          Success(pos, dtrs)
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Symbol(res.cats: _*)))
    else Failure
  }

  def parseCharSet(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 91) Success(next0 + 1, mutable.Buffer[GrammarParse](Position(next0))) else Failure
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = {
          val dtrs = mutable.Buffer[GrammarParse]()
          var pos = next1
          var keepGoing = true
          while (keepGoing && pos < to) {
            val res = {
              val res = parseHexChar(pos, to, input)
              if (res.success) res
              else {
                val res = parseEscapedChar(pos, to, input)
                if (res.success) res
                else {
                  val res =
                    if (pos >= input.size) Failure
                    else {
                      val c = input(pos)
                      if (c >= 97 && c <= 122)
                        Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                      else
                        Failure
                    }
                  if (res.success) res
                  else {
                    val res =
                      if (pos >= input.size) Failure
                      else {
                        val c = input(pos)
                        if (c >= 65 && c <= 90)
                          Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                        else
                          Failure
                      }
                    if (res.success) res
                    else {
                      val ch = input(pos)
                      if (ch < 44) {
                        if (ch < 39) {
                          if (ch < 34) {
                            if (ch == 32)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else if (ch == 33)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch > 34) {
                            if (ch == 35)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch == 34)
                            Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else Failure
                        } else if (ch > 39) {
                          if (ch < 42) {
                            if (ch == 40)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else if (ch == 41)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch > 42) {
                            if (ch == 43)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch == 42)
                            Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else Failure
                        } else if (ch == 39)
                          Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                        else Failure
                      } else if (ch > 44) {
                        if (ch < 60) {
                          if (ch < 47) {
                            if (ch == 45)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else if (ch == 46)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch > 47) {
                            if (ch == 59)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch == 47)
                            Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else Failure
                        } else if (ch > 60) {
                          if (ch < 94) {
                            if (ch == 62)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else if (ch == 63)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch > 94) {
                            if (ch == 95)
                              Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else Failure
                          } else if (ch == 94)
                            Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else Failure
                        } else if (ch == 60)
                          Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                        else Failure
                      } else if (ch == 44)
                        Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                      else Failure
                    }
                  }
                }
              }
            }
            if (!res.success) {
              keepGoing = false
            } else {
              dtrs ++= res.cats
              pos = res.pos
            }
          }
          Success(pos, dtrs)
        }
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos

          val res3 = {
            val ch = input(next2)
            if (ch == 93) Success(next2 + 1, mutable.Buffer[GrammarParse](Position(next2)))
            else Failure
          }
          if (res3.success) {
            dtrs ++= res3.cats
            Success(res3.pos, dtrs)
          } else {
            Failure
          }
        } else Failure
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](CharSet(res.cats: _*)))
    else Failure
  }

  def parseEscapedChar(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 92) Success(next0 + 1, mutable.Buffer[GrammarParse](Position(next0))) else Failure
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos

        val res2 = {
          val ch = input(next1)
          if (ch < 93) {
            if (ch < 91) {
              if (ch == 39) Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
              else Failure
            } else if (ch > 91) {
              if (ch == 92) Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
              else Failure
            } else if (ch == 91) Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
            else Failure
          } else if (ch > 93) {
            if (ch == 110) Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
            else if (ch == 116) Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
            else Failure
          } else if (ch == 93) Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
          else Failure
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Success(res2.pos, dtrs)
        } else {
          Failure
        }
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](EscapedChar(res.cats: _*)))
    else Failure
  }

  def parseHex(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res =
        if (from >= input.size) Failure
        else {
          val c = input(from)
          if (c >= 97 && c <= 102)
            Success(from + 1, mutable.Buffer[GrammarParse](Position(from)))
          else
            Failure
        }
      if (res.success) res
      else {
        val res =
          if (from >= input.size) Failure
          else {
            val c = input(from)
            if (c >= 65 && c <= 70)
              Success(from + 1, mutable.Buffer[GrammarParse](Position(from)))
            else
              Failure
          }
        if (res.success) res
        else if (from >= input.size) Failure
        else {
          val c = input(from)
          if (c >= 48 && c <= 57)
            Success(from + 1, mutable.Buffer[GrammarParse](Position(from)))
          else
            Failure
        }
      }
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Hex(res.cats: _*))) else Failure
  }

  def parseHexChar(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 92) Success(next0 + 1, mutable.Buffer[GrammarParse](Position(next0))) else Failure
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = {
          val ch = input(next1)
          if (ch == 120) Success(next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
          else Failure
        }
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos
          val res3 = parseHex(next2, to, input)
          if (res3.success) {
            dtrs ++= res3.cats
            val next3 = res3.pos
            val res4 = parseHex(next3, to, input)
            if (res4.success) {
              dtrs ++= res4.cats
              val next4 = res4.pos

              val res5 = {
                val res = {
                  val dtrs = mutable.Buffer[GrammarParse]()
                  val next0 = next4
                  val res1 = parseHex(next0, to, input)
                  if (res1.success) {
                    dtrs ++= res1.cats
                    val next1 = res1.pos

                    val res2 = parseHex(next1, to, input)
                    if (res2.success) {
                      dtrs ++= res2.cats
                      Success(res2.pos, dtrs)
                    } else {
                      Failure
                    }
                  } else Failure
                }
                if (res.success) res else Success(next4, mutable.Buffer[GrammarParse]())
              }
              if (res5.success) {
                dtrs ++= res5.cats
                Success(res5.pos, dtrs)
              } else {
                Failure
              }
            } else Failure
          } else Failure
        } else Failure
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](HexChar(res.cats: _*)))
    else Failure
  }

  def parseRange(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseTerminal(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = parseOptWS(next1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos
          val res3 = {
            val ch = input(next2)
            if (ch == 46) Success(next2 + 1, mutable.Buffer[GrammarParse](Position(next2)))
            else Failure
          }
          if (res3.success) {
            dtrs ++= res3.cats
            val next3 = res3.pos
            val res4 = {
              val ch = input(next3)
              if (ch == 46) Success(next3 + 1, mutable.Buffer[GrammarParse](Position(next3)))
              else Failure
            }
            if (res4.success) {
              dtrs ++= res4.cats
              val next4 = res4.pos
              val res5 = parseOptWS(next4, to, input)
              if (res5.success) {
                dtrs ++= res5.cats
                val next5 = res5.pos

                val res6 = parseTerminal(next5, to, input)
                if (res6.success) {
                  dtrs ++= res6.cats
                  Success(res6.pos, dtrs)
                } else {
                  Failure
                }
              } else Failure
            } else Failure
          } else Failure
        } else Failure
      } else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Range(res.cats: _*)))
    else Failure
  }

  def parseOptWS(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      var pos = from
      var keepGoing = true
      while (keepGoing && pos < to) {
        val res = {
          val ch = input(pos)
          if (ch < 10) {
            if (ch == 9) Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos))) else Failure
          } else if (ch > 10) {
            if (ch == 32) Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos))) else Failure
          } else if (ch == 10) Success(pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
          else Failure
        }
        if (!res.success) {
          keepGoing = false
        } else {
          dtrs ++= res.cats
          pos = res.pos
        }
      }
      Success(pos, dtrs)
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](OptWS(res.cats: _*)))
    else Failure
  }

  def parseOperator(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val ch = input(from)
      if (ch < 43) {
        if (ch == 42) Success(from + 1, mutable.Buffer[GrammarParse](Position(from))) else Failure
      } else if (ch > 43) {
        if (ch == 63) Success(from + 1, mutable.Buffer[GrammarParse](Position(from))) else Failure
      } else if (ch == 43) Success(from + 1, mutable.Buffer[GrammarParse](Position(from)))
      else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](Operator(res.cats: _*)))
    else Failure
  }

  def parseGapOperator(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val ch = input(from)
      if (ch == 35) Success(from + 1, mutable.Buffer[GrammarParse](Position(from))) else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](GapOperator(res.cats: _*)))
    else Failure
  }

  def parseAltOperator(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val ch = input(from)
      if (ch == 47) Success(from + 1, mutable.Buffer[GrammarParse](Position(from))) else Failure
    }
    if (res.success) Success(res.pos, mutable.Buffer[GrammarParse](AltOperator(res.cats: _*)))
    else Failure
  }
}
