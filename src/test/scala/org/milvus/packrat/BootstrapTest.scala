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
    testArg("( '?+*' )", Terms("?+*"))
  }
  
  test("Test expressions") {
    testExpression("'a'", Terms("a"))
    val range = Range('A', 'Z')
    testExpression("Range(A, Z)", range)
    testExpression("Star(Range(A, Z))", Star(range))
  }
  
  def testExpression(s: String, expr: Expr): Unit = {
    val (pos, e) = readExpression(s, 0)
    assert(pos == s.length)
    assert(e == expr)
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
