package org.milvus.packrat

import org.scalatest.FunSuite

import org.milvus.packrat.BootstrapGrammar._

class BootstrapTest extends FunSuite {

  test("Test character expressions") {
    testChar("'\\''", '\'')
    testChar("'\\n'", '\n')
  }
  
  test("Test single argument") {
    testArg("( '\\'' )", Terms("'"))
//    testArg("( '?+*' )", Terms("?+*"))
  }
  
  def testArg(s: String, exp: Expr): Unit = {
    val (pos, e) = readArg(s, 1)
    assert(s.length == pos)
    assert(e == exp)
  }
  
  def testChar(s: String, c: Char): Unit = {
    val (pos, ch) = readCharacter(s, 1)
    assert(s.length - 1 == pos)
    assert(c == ch)
  }
  
}
