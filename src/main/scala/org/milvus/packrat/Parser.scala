package org.milvus.packrat

import org.milvus.packrat.samples.ShallowParse
import org.milvus.packrat.samples.{Parser, ShallowParser}



object ParserMain extends App {
  
  sealed trait PrintNode
  case class Terminal(from: Int, to: Int) extends PrintNode
  case class NonTerminal(name: String, dtrs: Seq[PrintNode]) extends PrintNode
  
  def consolidateTerminals(nodes: Seq[PrintNode]): Seq[PrintNode] = {
    var out: List[PrintNode] = Nil
    var start = -1
    var end = -1
    for (node <- nodes) {
      node match {
        case Terminal(from, to) =>
          if (start < 0) {
            start = from
          }
          end = to
        case NonTerminal( _, _) =>
          if (start >= 0) {
            out = Terminal(start, end) :: out
            start = -1
          }
          out = node :: out
      }
    }
    if (start >= 0) out = Terminal(start, end) :: out
    out.reverse
  }
  
  def parseToPrintNode(parse: ShallowParse): PrintNode = {
    parse match {
      case ShallowParser.Position(n) => Terminal(n, n)
      case ShallowParser.NonTerminal(name, dtrs) =>
        NonTerminal(name, consolidateTerminals(dtrs.map(parseToPrintNode)))
    }
  }
  
  def printTree(tokenizerParse: ShallowParse): Unit = {
    val printTree = parseToPrintNode(tokenizerParse)
    //TODO: finish
    
  }
  
  val s = "VB PRP DT JJ NN NN IN NNP".split(" ").map(ShallowParser.codeForExternalSymbol(_))
  val result = ShallowParser.parseLongest(0, s.length, s)
  result match {
    case ShallowParser.ParseFailure => println("Parsing failed")
    case ShallowParser.ParseSuccess(_, parse) => println(parse)
  }
}