package org.meerkat.examples

import org.meerkat.Syntax._
import org.meerkat.parsers._
import Parsers._
import org.meerkat.tree._


object Start {
  
  /**
   * 
   * S ::= '(' S ')'
   *     | 'x'
   * 
   */
  
  val S: Nonterminal = 
    syn ( "(" ~~ S ~~ ")"
        | epsilon
        )
  
  
  def main(args: Array[String]): Unit = {
    parse(S, "()") match {
      case Right(x) => prettyPrint(x.root)
      case Left(x)  => println(x)
    }    
  }

  val Nt = NonterminalSymbol
  val Term = TerminalSymbol
   
  def prettyPrint(t: Tree): String = t match {
    case RuleNode(Rule(Nt("S"), List(Term("("), Nt("S"), Term(")"))), List(_, S, _)) => """|(
                                                   |  ${prettyPrint(S)}
                                                   |)""".stripMargin
                                                   
    case RuleNode(Rule(Nt("S"), List()), _)    => ""
      
    case _ => println(t); throw new RuntimeException
  }
  
}