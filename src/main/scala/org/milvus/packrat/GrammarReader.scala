package org.milvus.packrat

import java.io.InputStream
import java.nio.charset.StandardCharsets

import scala.io.Source 

/**
  * Main entry point for end user grammars.
  */
object GrammarReader {

  //TODO read different versions of grammars here (e.g., support for rich lexical items).
  
  private lazy val standardBSG = BootstrapGrammar.load
  
  def read(is: InputStream): Grammar = {
    val grammarString = Source.fromInputStream(is, StandardCharsets.UTF_8.name()).mkString
    val parseTree = Parser.parse(grammarString, standardBSG)
    Grammar(tree2rules(parseTree))
  }
  
  // Read a parse tree as obtained by BootstrapGrammar.peg and transform it into a sequence of rules (which we can then
  // use to instantiate a grammar). We do this extra step because it is easier to create a grammar from the intermediate
  // Rl format, than from a parse tree directly. We can afford to do this since this is a one time process and not
  // performance critical. Also, it is highly likely that the parse tree format will change in the future. The Rl type
  // on the other hand should remain fairly constant.
  private def tree2rules(tree: ParseTree): Seq[Rl] = {
    tree match {
      case ParseTreeNonTerminal(cat, dtrs) => {
        if (cat != standardBSG.getSymbolCode("Grammar")) {
          throw new IllegalArgumentException(s"Grammar error: Grammar expected, but found $tree")
        }
        println("Parsing successful!")
        trees2rules(dtrs)
      }
      case _ => throw new IllegalArgumentException(s"Grammar error: Grammar expected, but found $tree")
    }
  }
  
  private def trees2rules(trees: Seq[ParseTree]): Seq[Rl] = {
    Seq[Rl]()
  }

  def main(args: Array[String]): Unit = {
    val inputStream = classOf[Grammar].getClassLoader.getResourceAsStream("samples/test.peg")
    val grammar = read(inputStream)
    println(grammar)
  }
  
}
