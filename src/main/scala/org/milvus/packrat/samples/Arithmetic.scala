package org.milvus.packrat.samples

import org.milvus.packrat._

object Arithmetic {
  
  def sample1: Grammar = {
    
    val additive = Symbol("Additive", 0)
    val multitive = Symbol("Multitive", 1)
    val primary = Symbol("Primary", 2)
    val decimal = Symbol("Decimal", 3)
    
    val openParen = Terminal('(')
    val closeParen = Terminal(')')
    val plus = Terminal('+')
    val times = Terminal('*')
    
    val digits = for (i <- '0' to '9') yield Terminal(i)

    val addRule = Rule(additive, Alternative(Seq(Sequence(Seq(multitive, plus, additive)), multitive)))
    val multRule = Rule(multitive, Alternative(Seq(Sequence(Seq(primary, times, multitive)), primary)))
    val primaryRule = Rule(primary, Alternative(Seq(Sequence(Seq(openParen, additive, closeParen)), decimal)))
    val decimalRule = Rule(decimal, Alternative(digits))
    
    
    Grammar(additive, Seq[Rule](addRule, multRule, primaryRule,decimalRule))
  }

  def parseTest(s: String, g: Grammar): Unit = {
    val tree = Parser.parse(s.toCharArray, g)
    tree match {
      case EmptyParseTree => println(s"$s: false")
      case _ => println(tree.toString(g))
    }
  }
  
  def main(args: Array[String]): Unit = {
    val okInput = "(3+4)*(7*(4+2*7))"
    parseTest(okInput, sample1)
    val falseInput = "(3+4)*(7*(4+21))"
    parseTest(falseInput, sample1)

  }
  
}
