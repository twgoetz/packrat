package org.milvus.packrat

sealed trait Expr

case class Sym(name: String) extends Expr

case class Terms(chars: String) extends Expr

case class Range(from: Char, to: Char) extends Expr

case class Opt(e: Expr) extends Expr

case class Alt(es: Expr*) extends Expr

case class Sq(es: Expr*) extends Expr

case class Star(e: Expr) extends Expr

case class Plus(e: Expr) extends Expr

case class Rl(lhs: Sym, rhs: Expr)

object BootstrapGrammar {
  
  val exprName = "Expr"
  val parenExprName = "ParenExpr"
  val seqExprName = "SeqExpr"
  val simpleExprName = "SimpleExpr"
  val optWSName = "OptWS"
  val termName = "Terminal"
  val symName = "Symbol"
  val ruleName = "Rule"
  val grammarName = "Grammar"
  val charSetName = "CharSet"
  val escapedCharName = "EscapedChar"
  val hexCharName = "HexChar"
  val rangeName = "Range"
  val opName = "Operator"
  
  val exprSym = Sym(exprName)
  val parenSym = Sym(parenExprName)
  val seqSym = Sym(seqExprName)
  val simpleSym = Sym(simpleExprName)
  val optWSSym = Sym(optWSName)
  val symSym = Sym(symName)
  val ruleSym = Sym(ruleName)
  val grammarSym = Sym(grammarName)
  val charSetSym = Sym(charSetName)
  val escapedCharSym = Sym(escapedCharName)
  val hexCharSym = Sym(hexCharName)
  val rangeSym = Sym(rangeName)
  val opSym = Sym(opName)
  
  val escapedCharRule = Rl(escapedCharSym, Sq(Terms("\\"), Terms("nt\\")))
  val ws = Terms(" \n\t")
  val upper = Range('A', 'Z')
  val lower = Range('a', 'z')
  val digit = Range('0', '9')
  val hexDigit = Alt(Range('a', 'f'), Range('A', 'F'), digit)
  val hex = Sq(Terms("\\"), Terms("x"), hexDigit, hexDigit, Opt(Sq(hexDigit, hexDigit)))
  val hexCharRule = Rl(hexCharSym, hex)
  val alnum = Alt(upper, lower, digit)
  val other = Terms("-_()[]^!?.")
  val ch = Alt(alnum, other, escapedCharSym)
  val quot = Terms("'")
  val semi = Terms(";")
  val slash = Terms("/")
  val ops = Terms("?+*")
  val period = Terms(".")
  val rangeOp = Sq(period, period)
  val ruleOp = Sq(Terms("-"), Terms(">"))
  
  val opRule = Rl(opSym, ops)
  val term = Sq(quot, Alt(hexCharSym, ch), quot)
  val charSet = Sq(Terms("["), Plus(Alt(hexCharSym, ch)), Terms("]"))
  val charSetRule = Rl(charSetSym, charSet)
  val sym = Sq(Alt(upper, lower), Star(alnum))
  val symRule = Rl(symSym, sym)
  
  val optWSRule = Rl(optWSSym, Star(ws)) 
  
  val rangeExp = Sq(term, optWSSym, rangeOp, optWSSym, term)
  val rangeRule = Rl(rangeSym, rangeExp)
  val parenExprExp = Sq(Terms("("), optWSSym, exprSym, optWSSym, Terms(")"))
  val simpleExp = Sq(Alt(rangeSym, term, symSym, charSetSym), optWSSym, Opt(opSym))
  val simpleExprRule = Rl(simpleSym, simpleExp)
  
  val seqExprExp = Sq(simpleSym, Star(Sq(optWSSym, seqSym)))
  val seqExpRule = Rl(seqSym, seqExprExp)
  
  val expRule = Rl(exprSym, Sq(seqSym, Star(Sq(optWSSym, slash, optWSSym, exprSym))))
  
  val ruleExp = Sq(optWSSym, symSym, optWSSym, ruleOp, optWSSym, exprSym, optWSSym, semi)
  val ruleRule = Rl(ruleSym, ruleExp)
  
  val grammarRule = Rl(grammarSym, Sq(Plus(ruleSym), optWSSym))

  def rules = Seq[Rl](grammarRule, ruleRule, expRule, seqExpRule, simpleExprRule, optWSRule, symRule, escapedCharRule, 
    charSetRule, hexCharRule, rangeRule, opRule)
//  def rules = Seq[Rl](simpleExprRule, optWSRule)
//  def rules = Seq[Rl](Rl(simpleSym, Sq(sym, optWSSym)), optWSRule)
  
  def grammar = new Grammar(rules)
  
  def tree2grammar(tree: ParseTree, g: Grammar): Grammar = {
    tree match {
      case ParseTreeNonTerminal(sym, dtrs) => {
        g.symbolList(sym).name match {
          case `grammarName` => tree2grammar(dtrs(0), g)
        }
      }
    }
  }
  
  def rule2rule(tree: ParseTree, g: Grammar): Option[Rl] = {
    val ruleSymCode = g.getSymbolCode(ruleName)
    tree match {
      case ParseTreeNonTerminal(`ruleSymCode`, dtrs) => {
        val sym = sym2sym(dtrs(1)).get
        val exp = tree2exp(dtrs(5), g).get
        Some(Rl(sym, exp))
      }
      case _ => None
    }
  }
  
  def tree2exp(tree: ParseTree, g: Grammar): Option[Expr] = {
    tree match {
      case ParseTreeNonTerminal(cat, dtrs) => {
        val catName = g.symbolList(cat).name
        catName match {
          case `parenExprName` => tree2exp(dtrs(2), g)
          case _ => None
        }
      }
      case _ => None
    }
  }
  
  def sym2sym(tree: ParseTree): Option[Sym] = {
    tree match {
      case ParseTreeNonTerminal(_, dtrs) => {
        val name = dtrs
          .map({case ParseTreeTerminal(ch) => ch.toChar})
          .mkString("")
        Some(Sym(name))
      }
      case _ => None
    }
    
  }
  
  // parenExp = '(' exp ')'
  // exp = seqExp ( '/' exp )*
  // seqExp = simpleExp seqExp*
  // simpleExp = ( parenExp / Term / Symbol / Range ) Op*

  def main(args: Array[String]): Unit = {
    val text = "foo -> bar+ 'x'..'z'; bar -> 'a'/'\\x12ab'/'\\n'; "
//    val text = "foo".toCharArray
    val g = grammar
    val parseResult = Parser.parse(text, g)
    println(parseResult.toString(g))
  }
  
}
