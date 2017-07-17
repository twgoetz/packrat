package org.milvus.packrat.samples.grammar

import org.milvus.packrat.Parser
import scala.collection.mutable

sealed trait GrammarParse

object GrammarParser extends Parser[GrammarParse] {

  case class Grammar(dtrs: GrammarParse*) extends GrammarParse
  case class Operator(dtrs: GrammarParse*) extends GrammarParse
  case class GapOperator(dtrs: GrammarParse*) extends GrammarParse
  case class Expr(dtrs: GrammarParse*) extends GrammarParse
  case class Alternative(dtrs: GrammarParse*) extends GrammarParse
  case class SeqExpr(dtrs: GrammarParse*) extends GrammarParse
  case class ParenExpr(dtrs: GrammarParse*) extends GrammarParse
  case class GapExpr(dtrs: GrammarParse*) extends GrammarParse
  case class SimpleExpr(dtrs: GrammarParse*) extends GrammarParse
  case class OptWS(dtrs: GrammarParse*) extends GrammarParse
  case class Terminal(dtrs: GrammarParse*) extends GrammarParse
  case class Symbol(dtrs: GrammarParse*) extends GrammarParse
  case class Rule(dtrs: GrammarParse*) extends GrammarParse
  case class CharSet(dtrs: GrammarParse*) extends GrammarParse
  case class EscapedChar(dtrs: GrammarParse*) extends GrammarParse
  case class Hex(dtrs: GrammarParse*) extends GrammarParse
  case class HexChar(dtrs: GrammarParse*) extends GrammarParse
  case class Range(dtrs: GrammarParse*) extends GrammarParse
  case class Position(pos: Int) extends GrammarParse

  case class Result(success: Boolean, pos: Int, cats: mutable.Buffer[GrammarParse])
  val failure = Result(false, 0, mutable.Buffer())

  override def parseLongest(from: Int, to: Int, input: Seq[Int]): ParseResult = {
    val res = parseGrammar(from, to, input)
    if (res.success) ParseSuccess(res.pos, res.cats(0))
    else ParseFailure
  }

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
      Result(true, pos, dtrs)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Grammar(res.cats: _*)))
    else failure
  }

  def parseOperator(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val ch = input(from)
      if (ch < 43) {
        if (ch == 42) Result(true, from + 1, mutable.Buffer[GrammarParse](Position(from)))
        else failure
      } else if (ch > 43) {
        if (ch == 63) Result(true, from + 1, mutable.Buffer[GrammarParse](Position(from)))
        else failure
      } else if (ch == 43) Result(true, from + 1, mutable.Buffer[GrammarParse](Position(from)))
      else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Operator(res.cats: _*)))
    else failure
  }

  def parseGapOperator(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val ch = input(from)
      if (ch == 35) Result(true, from + 1, mutable.Buffer[GrammarParse](Position(from)))
      else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](GapOperator(res.cats: _*)))
    else failure
  }

  def parseExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseSeqExpr(next0, to, input)
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos

        val res2 = parseAlternative(next1, to, input)
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Expr(res.cats: _*)))
    else failure
  }

  def parseAlternative(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      var pos = from
      var keepGoing = true
      while (keepGoing && pos < to) {
        val res = {
          val dtrs = mutable.Buffer[GrammarParse]()
          val next0 = pos
          val res1 = parseOptWS(next0, to, input)
          if (res1.success) {
            dtrs ++= res1.cats
            val next1 = res1.pos
            val res2 = {
              val ch = input(next1)
              if (ch == 47) Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
              else failure
            }
            if (res2.success) {
              dtrs ++= res2.cats
              val next2 = res2.pos
              val res3 = parseOptWS(next2, to, input)
              if (res3.success) {
                dtrs ++= res3.cats
                val next3 = res3.pos

                val res4 = parseExpr(next3, to, input)
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
        if (!res.success) {
          keepGoing = false
        } else {
          dtrs ++= res.cats
          pos = res.pos
        }
      }
      Result(true, pos, dtrs)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Alternative(res.cats: _*)))
    else failure
  }

  def parseSeqExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = parseParenExpr(next0, to, input)
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

                val res2 = parseExpr(next1, to, input)
                if (res2.success) {
                  dtrs ++= res2.cats
                  Result(true, res2.pos, dtrs)
                } else {
                  failure
                }
              } else failure
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
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](SeqExpr(res.cats: _*)))
    else failure
  }

  def parseParenExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = {
        val dtrs = mutable.Buffer[GrammarParse]()
        val next0 = from
        val res1 = {
          val ch = input(next0)
          if (ch == 40) Result(true, next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
          else failure
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
                  if (ch == 41)
                    Result(true, next4 + 1, mutable.Buffer[GrammarParse](Position(next4)))
                  else failure
                }
                if (res5.success) {
                  dtrs ++= res5.cats
                  Result(true, res5.pos, dtrs)
                } else {
                  failure
                }
              } else failure
            } else failure
          } else failure
        } else failure
      }
      if (res.success) res
      else parseSimpleExpr(from, to, input)
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](ParenExpr(res.cats: _*)))
    else failure
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
            Result(true, res3.pos, dtrs)
          } else {
            failure
          }
        } else failure
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](GapExpr(res.cats: _*)))
    else failure
  }

  def parseSimpleExpr(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res = parseGapExpr(from, to, input)
      if (res.success) res
      else {
        val dtrs = mutable.Buffer[GrammarParse]()
        val next0 = from
        val res1 = {
          val res = parseRange(next0, to, input)
          if (res.success) res
          else {
            val res = parseTerminal(next0, to, input)
            if (res.success) res
            else {
              val res = parseSymbol(next0, to, input)
              if (res.success) res
              else parseCharSet(next0, to, input)
            }
          }
        }
        if (res1.success) {
          dtrs ++= res1.cats
          val next1 = res1.pos
          val res2 = parseOptWS(next1, to, input)
          if (res2.success) {
            dtrs ++= res2.cats
            val next2 = res2.pos

            val res3 = {
              val res = parseOperator(next2, to, input)
              if (res.success) res else Result(true, next2, mutable.Buffer[GrammarParse]())
            }
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
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](SimpleExpr(res.cats: _*)))
    else failure
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
            if (ch == 9) Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
            else failure
          } else if (ch > 10) {
            if (ch == 32) Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
            else failure
          } else if (ch == 10) Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
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
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](OptWS(res.cats: _*)))
    else failure
  }

  def parseTerminal(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 39) Result(true, next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
        else failure
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
                if (next1 >= input.size) failure
                else {
                  val c = input(next1)
                  if (c >= 97 && c <= 122)
                    Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                  else
                    failure
                }
              if (res.success) res
              else {
                val res =
                  if (next1 >= input.size) failure
                  else {
                    val c = input(next1)
                    if (c >= 65 && c <= 90)
                      Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                    else
                      failure
                  }
                if (res.success) res
                else {
                  val ch = input(next1)
                  if (ch < 63) {
                    if (ch < 41) {
                      if (ch == 33)
                        Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else if (ch == 40)
                        Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else failure
                    } else if (ch > 41) {
                      if (ch == 45)
                        Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else if (ch == 46)
                        Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else failure
                    } else if (ch == 41)
                      Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                    else failure
                  } else if (ch > 63) {
                    if (ch < 94) {
                      if (ch == 91)
                        Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else if (ch == 93)
                        Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else failure
                    } else if (ch > 94) {
                      if (ch == 95)
                        Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                      else failure
                    } else if (ch == 94)
                      Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                    else failure
                  } else if (ch == 63)
                    Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
                  else failure
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
            if (ch == 39) Result(true, next2 + 1, mutable.Buffer[GrammarParse](Position(next2)))
            else failure
          }
          if (res3.success) {
            dtrs ++= res3.cats
            Result(true, res3.pos, dtrs)
          } else {
            failure
          }
        } else failure
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Terminal(res.cats: _*)))
    else failure
  }

  def parseSymbol(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val res =
          if (next0 >= input.size) failure
          else {
            val c = input(next0)
            if (c >= 97 && c <= 122)
              Result(true, next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
            else
              failure
          }
        if (res.success) res
        else if (next0 >= input.size) failure
        else {
          val c = input(next0)
          if (c >= 65 && c <= 90)
            Result(true, next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
          else
            failure
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
                if (pos >= input.size) failure
                else {
                  val c = input(pos)
                  if (c >= 97 && c <= 122)
                    Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                  else
                    failure
                }
              if (res.success) res
              else {
                val res =
                  if (pos >= input.size) failure
                  else {
                    val c = input(pos)
                    if (c >= 65 && c <= 90)
                      Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                    else
                      failure
                  }
                if (res.success) res
                else if (pos >= input.size) failure
                else {
                  val c = input(pos)
                  if (c >= 48 && c <= 57)
                    Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                  else
                    failure
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
          Result(true, pos, dtrs)
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Symbol(res.cats: _*)))
    else failure
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
              if (ch == 45) Result(true, next3 + 1, mutable.Buffer[GrammarParse](Position(next3)))
              else failure
            }
            if (res4.success) {
              dtrs ++= res4.cats
              val next4 = res4.pos
              val res5 = {
                val ch = input(next4)
                if (ch == 62) Result(true, next4 + 1, mutable.Buffer[GrammarParse](Position(next4)))
                else failure
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
                          Result(true, next8 + 1, mutable.Buffer[GrammarParse](Position(next8)))
                        else failure
                      }
                      if (res9.success) {
                        dtrs ++= res9.cats
                        val next9 = res9.pos

                        val res10 = parseOptWS(next9, to, input)
                        if (res10.success) {
                          dtrs ++= res10.cats
                          Result(true, res10.pos, dtrs)
                        } else {
                          failure
                        }
                      } else failure
                    } else failure
                  } else failure
                } else failure
              } else failure
            } else failure
          } else failure
        } else failure
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Rule(res.cats: _*)))
    else failure
  }

  def parseCharSet(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 91) Result(true, next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
        else failure
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
                    if (pos >= input.size) failure
                    else {
                      val c = input(pos)
                      if (c >= 97 && c <= 122)
                        Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                      else
                        failure
                    }
                  if (res.success) res
                  else {
                    val res =
                      if (pos >= input.size) failure
                      else {
                        val c = input(pos)
                        if (c >= 65 && c <= 90)
                          Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                        else
                          failure
                      }
                    if (res.success) res
                    else {
                      val ch = input(pos)
                      if (ch < 44) {
                        if (ch < 39) {
                          if (ch < 33) {
                            if (ch == 32)
                              Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else failure
                          } else if (ch > 33) {
                            if (ch == 34)
                              Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                            else failure
                          } else if (ch == 33)
                            Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else failure
                        } else if (ch > 39) {
                          if (ch == 40)
                            Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else if (ch == 41)
                            Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else failure
                        } else if (ch == 39)
                          Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                        else failure
                      } else if (ch > 44) {
                        if (ch < 63) {
                          if (ch == 45)
                            Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else if (ch == 46)
                            Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else failure
                        } else if (ch > 63) {
                          if (ch == 94)
                            Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else if (ch == 95)
                            Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                          else failure
                        } else if (ch == 63)
                          Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                        else failure
                      } else if (ch == 44)
                        Result(true, pos + 1, mutable.Buffer[GrammarParse](Position(pos)))
                      else failure
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
          Result(true, pos, dtrs)
        }
        if (res2.success) {
          dtrs ++= res2.cats
          val next2 = res2.pos

          val res3 = {
            val ch = input(next2)
            if (ch == 93) Result(true, next2 + 1, mutable.Buffer[GrammarParse](Position(next2)))
            else failure
          }
          if (res3.success) {
            dtrs ++= res3.cats
            Result(true, res3.pos, dtrs)
          } else {
            failure
          }
        } else failure
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](CharSet(res.cats: _*)))
    else failure
  }

  def parseEscapedChar(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 92) Result(true, next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
        else failure
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos

        val res2 = {
          val ch = input(next1)
          if (ch < 93) {
            if (ch < 91) {
              if (ch == 39) Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
              else failure
            } else if (ch > 91) {
              if (ch == 92) Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
              else failure
            } else if (ch == 91)
              Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
            else failure
          } else if (ch > 93) {
            if (ch == 110) Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
            else if (ch == 116)
              Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
            else failure
          } else if (ch == 93)
            Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
          else failure
        }
        if (res2.success) {
          dtrs ++= res2.cats
          Result(true, res2.pos, dtrs)
        } else {
          failure
        }
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](EscapedChar(res.cats: _*)))
    else failure
  }

  def parseHex(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val res =
        if (from >= input.size) failure
        else {
          val c = input(from)
          if (c >= 97 && c <= 102)
            Result(true, from + 1, mutable.Buffer[GrammarParse](Position(from)))
          else
            failure
        }
      if (res.success) res
      else {
        val res =
          if (from >= input.size) failure
          else {
            val c = input(from)
            if (c >= 65 && c <= 70)
              Result(true, from + 1, mutable.Buffer[GrammarParse](Position(from)))
            else
              failure
          }
        if (res.success) res
        else if (from >= input.size) failure
        else {
          val c = input(from)
          if (c >= 48 && c <= 57)
            Result(true, from + 1, mutable.Buffer[GrammarParse](Position(from)))
          else
            failure
        }
      }
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Hex(res.cats: _*)))
    else failure
  }

  def parseHexChar(from: Int, to: Int, input: Seq[Int]): Result = {
    val res = {
      val dtrs = mutable.Buffer[GrammarParse]()
      val next0 = from
      val res1 = {
        val ch = input(next0)
        if (ch == 92) Result(true, next0 + 1, mutable.Buffer[GrammarParse](Position(next0)))
        else failure
      }
      if (res1.success) {
        dtrs ++= res1.cats
        val next1 = res1.pos
        val res2 = {
          val ch = input(next1)
          if (ch == 120) Result(true, next1 + 1, mutable.Buffer[GrammarParse](Position(next1)))
          else failure
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
                      Result(true, res2.pos, dtrs)
                    } else {
                      failure
                    }
                  } else failure
                }
                if (res.success) res else Result(true, next4, mutable.Buffer[GrammarParse]())
              }
              if (res5.success) {
                dtrs ++= res5.cats
                Result(true, res5.pos, dtrs)
              } else {
                failure
              }
            } else failure
          } else failure
        } else failure
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](HexChar(res.cats: _*)))
    else failure
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
            if (ch == 46) Result(true, next2 + 1, mutable.Buffer[GrammarParse](Position(next2)))
            else failure
          }
          if (res3.success) {
            dtrs ++= res3.cats
            val next3 = res3.pos
            val res4 = {
              val ch = input(next3)
              if (ch == 46) Result(true, next3 + 1, mutable.Buffer[GrammarParse](Position(next3)))
              else failure
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
                  Result(true, res6.pos, dtrs)
                } else {
                  failure
                }
              } else failure
            } else failure
          } else failure
        } else failure
      } else failure
    }
    if (res.success) Result(true, res.pos, mutable.Buffer[GrammarParse](Range(res.cats: _*)))
    else failure
  }
}
