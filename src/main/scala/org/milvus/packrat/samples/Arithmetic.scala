package org.milvus.packrat.samples

import org.milvus.packrat._

object Arithmetic {
  
  def sample1: Grammar = {
    
    val additive = Sym("Additive")
    val multitive = Sym("Multitive")
    val primary = Sym("Primary")
    val decimal = Sym("Decimal")
    
    val openParen = Terms("(")
    val closeParen = Terms(")")
    val plus = Terms("+")
    val times = Terms("*")
    
    val digits = Range('0', '9')

    val addRule = Rl(additive, Alt(Sq(multitive, plus, additive), multitive))
    val multRule = Rl(multitive, Alt(Sq(primary, times, multitive), primary))
    val primaryRule = Rl(primary, Alt(Sq(openParen, additive, closeParen), decimal))
    val decimalRule = Rl(decimal, Alt(digits))
    
    Grammar(Seq[Rl](addRule, multRule, primaryRule,decimalRule))
  }

  def parseTest(s: String, g: Grammar): Unit = {
    val tree = Parser.parse(s, g)
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
