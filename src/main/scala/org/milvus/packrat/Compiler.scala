package org.milvus.packrat

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util

import com.sun.org.apache.xalan.internal.xsltc.compiler.sym
import com.sun.org.apache.xpath.internal.operations.Variable
import org.scalafmt.config.ScalafmtConfig

case class Compiler(packageName: String, parserClassName: String, parseTreeClassName: String) {

  val bufferClass = s"mutable.Buffer[$parseTreeClassName]"

  case class TranslationResult(trans: String, fromCount: Int)

  val staticImports =
    """import org.milvus.packrat.Parser
      |import scala.collection.mutable
      |
      |""".stripMargin

  // Gather symbols that have no rules defined for them. Those symbols are references to externally
  // generated input, such as POS tags. Assign a (negative) integer to each such symbol, and
  // provide conversion utilities.
  def collectExternalSymbols(rules: Seq[Rl]): Seq[String] = {
    
    val grammarSymbols: Set[String] = rules
      .map(r => r.lhs.name)
      .toSet
    
    def collect(e: Expr): Seq[String] = {
      e match {
        case Sym(name) => if (grammarSymbols.contains(name)) Seq() else Seq(name)
        case Opt(ex) => collect(e)
        case Alt(ex @ _*) => ex.flatMap(collect)
        case Sq(ex @ _*) => ex.flatMap(collect)
        case Star(ex) => collect(ex)
        case Plus(ex) => collect(ex)
        case Gap(ex) => collect(ex)
        case _ => Seq()
      }
    }
    
    rules
      .flatMap(r => collect(r.rhs))
      .distinct
  }
  
  def compile(rules: Seq[Rl]): String = {
    val packageDecl = s"package $packageName\n\n"
    val parseTreeDecl = s"sealed trait $parseTreeClassName\n\n"
    val categoriesDecl = rules
      .map(_.lhs.name)
      .map(name => s"case class $name(dtrs: $parseTreeClassName*) extends $parseTreeClassName\n")
      .mkString
    val classDecls = s"case class Position(pos: Int) extends $parseTreeClassName\n\n" +
      s"case class Result(success: Boolean, pos: Int, cats: mutable.Buffer[$parseTreeClassName])\n" +
      "val failure = Result(false, 0, mutable.Buffer())\n\n"
    val externalSymbolDecl = "sealed abstract class ExternalSymbol(val name: String, val index: Int)\n"
    val unknownSymbolDecl = "case object UnknownSymbol extends ExternalSymbol(\"\", -1)\n"
    val extSyms = collectExternalSymbols(rules)
    val externalSymbolsDecl = extSyms
      .zipWithIndex
      .map(p => {
        s"case object ${p._1} extends ExternalSymbol(" + "\"" + p._1 + "\"" + s", ${-2 - p._2})"
      })
      .mkString("\n")
    val externalSymbolList = "Seq(" + extSyms.mkString(", ") + ")"
    val externalSymbolMap = s"\n\nprivate val externalSymbolMap: Map[String, ExternalSymbol] = $externalSymbolList" +
      ".foldLeft(Map[String, ExternalSymbol]())((map, sym) => map + " +
      "(sym.name -> sym))\n\n"
    val externalSymbolCodeFun = "override def codeForExternalSymbol(sym: String) = externalSymbolMap\n  " +
      "  .getOrElse(sym, UnknownSymbol)\n    .index\n\n"
    val externalSymbolDefinition = externalSymbolDecl + unknownSymbolDecl + externalSymbolsDecl +
      externalSymbolMap + externalSymbolCodeFun
    val classHeader = s"\nobject $parserClassName extends Parser[$parseTreeClassName] {\n\n"
    val startSymbol = rules(0).lhs.name
    val abstractFunImpl = s"override def parseLongest(from: Int, to: Int, input: Seq[Int]): ParseResult = {\n" +
      s"val res = parse$startSymbol(from, to, input)\n if (res.success) ParseSuccess(res.pos, res.cats(0))\n" +
      "else ParseFailure\n }\n\n"
    val extSymMap = extSyms
      .zipWithIndex
      .foldLeft(Map[String, Int]())((map, sym) => map + (sym._1 -> (-2 - sym._2)))
    val functionDecls = rules
      .map(rl => compile(rl, extSymMap))
      .mkString("\n")
    val classFooter = "}\n"
    packageDecl + staticImports + parseTreeDecl + classHeader + categoriesDecl + classDecls + externalSymbolDefinition + abstractFunImpl +
      functionDecls + classFooter
  }

  def compile(rule: Rl, externalSymbols: Map[String, Int]): String = {
    val lhs = rule.lhs.name
    val funHeader = s"def parse$lhs(from: Int, to: Int, input: Seq[Int]): Result = {\n val res = "
    val funBody = compile(rule.rhs, "from", externalSymbols)
    val funFooter = s"if (res.success) Result(true, res.pos, $bufferClass($lhs(res.cats: _*))) else failure\n}\n"
    funHeader + funBody + funFooter
  }

  def compile(expr: Expr, fromVar: String, externalSymbols: Map[String, Int]): String = {
    expr match {
      case Range(from, to) => compileRange(from, to, fromVar)
      case Alt(es@_*) => compileAlternative(es, fromVar, externalSymbols)
      case Sym(name) =>
        val index = externalSymbols.getOrElse(name, 0)
        if (index == 0) {
          s"parse$name($fromVar, to, input)\n"
        } else {
          compileSet(Array(index), fromVar)
        }
      case Plus(e) => compilePlus(e, fromVar, externalSymbols)
      case Star(e) => compileStar(e, fromVar, externalSymbols)
      case Terms(str) => compileSet(str.toCharArray.map(_.toInt), fromVar)
      case Sq(es@_*) => compileSeq(es, fromVar, externalSymbols)
      case Opt(e) => compileOptional(e, fromVar, externalSymbols)
      case Gap(e) => compileGap(e, fromVar, externalSymbols)
      case _ => "failure // Unknown grammar expression\n"
    }
  }

  def compileOptional(expr: Expr, fromVar: String, externalSymbols: Map[String, Int]): String = {
    s"{\nval res = ${compile(expr, fromVar, externalSymbols)} if (res.success) res else Result(true, $fromVar, $bufferClass())\n}\n"
  }

  def compileSeq(exprs: Seq[Expr], fromVar: String, externalSymbols: Map[String, Int]): String = {
    val s1 = s"{\n val dtrs = mutable.Buffer[$parseTreeClassName]()\n val next0 = $fromVar\n"

    def cs(start: Int, end: Int): String = {
      if (start == end) {
        val count = start + 1
        val c1 = s"\n val res$count = ${compile(exprs(start), "next" + start, externalSymbols)}"
        val c2 = s"if (res$count.success) {\n dtrs ++= res$count.cats\n Result(true, res$count.pos, dtrs)\n } else {\n failure\n }\n"
        c1 + c2
      } else {
        val count = start + 1
        val c1 = s"val res$count = ${compile(exprs(start), "next" + start, externalSymbols)}"
        val c2 = s"if (res$count.success) {\n dtrs ++= res$count.cats\n val next$count = res$count.pos\n"
        val c4 = cs(start + 1, end)
        val c5 = "}\n else failure\n"
        c1 + c2 + c4 + c5
      }
    }

    val s2 = "}\n"
    s1 + cs(0, exprs.size - 1) + s2
  }

  def compileSet(set: Array[Int], fromVar: String): String = {
    util.Arrays.sort(set)
    if (set.isEmpty) "failure"
    else {

      def cs(start: Int, end: Int): String = {
        val size = end - start
        if (size == 1) s"if (ch == ${set(start)}) Result(true, $fromVar + 1, $bufferClass(Position($fromVar))) else failure\n"
        else if (size == 2) {
          val c1 = s"if (ch == ${set(start)}) Result(true, $fromVar + 1, $bufferClass(Position($fromVar)))\n"
          val c2 = s"else if(ch == ${set(start + 1)}) Result(true, $fromVar + 1, $bufferClass(Position($fromVar)))\n else failure\n "
          c1 + c2
        } else {
          val middlePos = start + (size / 2)
          val dec = set(middlePos)
          val c1 = s"if (ch < $dec) {\n ${cs(start, middlePos)} }\n"
          val c2 = s"else if (ch > $dec) {\n ${cs(middlePos + 1, end)} } \n"
          val c3 = s"else if (ch == $dec) Result(true, $fromVar + 1, $bufferClass(Position($fromVar)))\n else failure\n"
          c1 + c2 + c3
        }
      }

      s"{\nval ch = input($fromVar)\n" + cs(0, set.size) + "}\n"
    }
  }

  def compileStar(expr: Expr, fromVar: String, externalSymbols: Map[String, Int]): String = {
    val pos = TmpPos.next
    val p1 = s"{\nval dtrs = mutable.Buffer[$parseTreeClassName]()\n var $pos = $fromVar\n var keepGoing = true\n while (keepGoing && $pos < to) {\n"
    val p2 = s"val res = ${compile(expr, pos, externalSymbols)} if (!res.success) {\n keepGoing = false\n}\n"
    val p3 = s"else {\n dtrs ++= res.cats\n $pos = res.pos\n}\n}\n Result(true, $pos, dtrs)\n}\n"
    p1 + p2 + p3
  }
  
  def compilePlus(expr: Expr, fromVar: String, externalSymbols: Map[String, Int]): String = {
    val pos = TmpPos.next
    val expression = compile(expr, fromVar, externalSymbols)
    val p1 = s"{\n val res = $expression"
    val p2 = "if (!res.success) {\n res\n} else {\n "
    val p3 = s"val dtrs = res.cats\n var $pos = res.pos\n var keepGoing = true\n while (keepGoing && $pos < to) {\n"
    val p4 = s"val res = ${compile(expr, pos, externalSymbols)} if (!res.success) {\n keepGoing = false\n} else {\n"
    val p5 = s"dtrs ++= res.cats\n $pos = res.pos\n}\n}\n Result(true, $pos, dtrs)\n}\n}\n"
    p1 + p2 + p3 + p4 + p5
  }
  
  def compileGap(expr: Expr, fromVar: String, externalSymbols: Map[String, Int]): String = {
    val pos = TmpPos.next
    val expression = compile(expr, pos, externalSymbols)
    val p1 = s"{\n var $pos = $fromVar\nvar res = $expression"
    val p2 = s"while (!res.success && $pos < to) {\n "
    val p3 = s"$pos = $pos + 1\nres = $expression}\nres\n}\n"
    p1 + p2 + p3
  }
  
  def compileAlternative(exprs: Seq[Expr], fromVar: String, externalSymbols: Map[String, Int]): String = {

    val max = exprs.size - 1

    def compileAlternative(pos: Int): String = {
      if (pos == max) {
        compile(exprs(pos), fromVar, externalSymbols)
      } else {
        val next = compileAlternative(pos + 1)
        val a1 = s"{\n val res = ${compile(exprs(pos), fromVar, externalSymbols)} if (res.success) res\n"
        val a2 = s"else $next}\n"
        a1 + a2
      }
    }

    compileAlternative(0)
  }

  def compileRange(from: Int, to: Int, fromVar: String): String = {
    s"if ($fromVar >= input.size) failure\nelse {\n val c = input($fromVar)\n if (c >= $from && c <= $to)\n" +
      s"Result(true, $fromVar + 1, $bufferClass(Position($fromVar)))\n else\n failure}\n"
  }
  
  object TmpPos {
    
    private var i: Int = 0
    
    def next: String = {
      this.synchronized {
        val x = i
        i = i + 1
        "pos" + x
      }
    }
    
  }

}

object Compiler {

  def main(args: Array[String]): Unit = {
    //    val packageName = "org.milvus.packrat.samples.grammar"
    //    val parserClassName = "GrammarParser"
    //    val parseTreeClassName = "GrammarParse"

    val packageName = "org.milvus.packrat.samples"
    val parserClassName = "ShallowParser"
    val parseTreeClassName = "ShallowParse"

    val inputStream = classOf[Compiler].getClassLoader.getResourceAsStream("samples/ShallowParser.peg")
//    val inputStream = classOf[Compiler].getClassLoader.getResourceAsStream("BootstrapGrammar.peg")
    val rules = GrammarReader.parseGrammar(inputStream)

    //    val rules = BootstrapGrammar.loadRules()
    rules.foreach(println(_))
    val compiler = Compiler(packageName, parserClassName, parseTreeClassName)
    val unformatted = compiler.compile(rules)
    val config = ScalafmtConfig(maxColumn = 100)
    val formatted = org.scalafmt.Scalafmt.format(unformatted, config).get
    println(formatted)
  }

}

object BootstrapCompiler {
  def main(args: Array[String]): Unit = {
    val packageName = "org.milvus.packrat.samples.grammar"
    val parserClassName = "GrammarParser"
    val parseTreeClassName = "GrammarParse"

    val rules = BootstrapGrammar.loadRules()
    rules.foreach(println(_))
    val compiler = Compiler(packageName, parserClassName, parseTreeClassName)
    val unformatted = compiler.compile(rules)
    val config = ScalafmtConfig(maxColumn = 100)
    val formatted = org.scalafmt.Scalafmt.format(unformatted, config).get
    println(formatted)
    
    val outFile = new File("src/main/scala/org/milvus/packrat/samples/grammar/GrammarParser.scala")
    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile), StandardCharsets.UTF_8))
    out.write(formatted)
    out.close()
  }
}