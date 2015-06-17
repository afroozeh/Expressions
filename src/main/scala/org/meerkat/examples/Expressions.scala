package org.meerkat.examples

import org.meerkat.Syntax._
import org.meerkat.parsers._
import org.meerkat.parsers.Parsers._
import org.meerkat.parsers.OperatorParsers._
import org.meerkat.sppf.TreeBuilder
import org.meerkat.util.Input
import org.meerkat.util.visualization._
import org.meerkat.sppf.SemanticActionExecutor
import org.meerkat.sppf.SemanticAction

sealed trait E

case class Add(l: E, r: E) extends E
case class Mul(l: E, r: E) extends E
case class Sub(l: E, r: E) extends E
case class Div(l: E, r: E) extends E
case class Neg(l: E)       extends E
case class Pow(l: E, r: E) extends E
case class Num(n: Int)     extends E


object Test {
 
  val toInt: String => Int = x => x.toInt
  
//  E ::= E '+' E
//    | E '*' E
//    | E '-' E
//    | E '/' E
//    | '-' E
//    | E '^' E
//    | Int
  


def Octets(n: Int, p: Nonterminal) 
  = n match {
      case 1 => syn ( p )
      case _ => syn ( (1 to n-2).foldLeft(p ~ p)((q,_) => q ~ p) )
    }
  
//  val E: Nonterminal
//  = syn ( E ~ "^" ~ E
//        | "-" ~ E
//        | E ~ "*" ~ E 
//        | E ~ "/" ~ E 
//        | E ~ "+" ~ E 
//        | E ~ "-" ~ E
//        | "("  ~ E ~ ")"
//        | "[0-9]".r 
//        )
  
  val E: OperatorNonterminal & E
  = syn (  right { E ~ "^" ~ E } & { case (x, y) => Pow(x, y) }
        |>       "-" ~ E         & { Neg(_).asInstanceOf[E] }
        |> left ( E ~ "*" ~ E    & { case (x, y) => Mul(x, y) } 
        |         E ~ "/" ~ E    & { case (x, y) => Div(x, y) }) 
        |> left ( E ~ "+" ~ E    & { case (x, y) => Add(x, y) } 
        |         E ~ "-" ~ E    & { case (x, y) => Sub(x, y) })
        | "("  ~ E ~ ")"
        | "[0-9]".r              ^ { s => Num(toInt(s)).asInstanceOf[E] } 
        )
        
   val S = syn ( A ~ B ~ C)
   
   val A = syn ("a")
   
   val B = syn ("b")
   
   val C = syn ("c")
   
    def main(args: Array[String]): Unit = {
//      implicit val input = Input("1*2+3-(1+7+-8)^3")
//      val s = parse(E($), input)
//      val t = TreeBuilder.build(s.right.get.sppf, false)
//      println(SemanticAction.execute(s.right.get.sppf))
//      visualize(t, input)        
    }
   
//    def main(args: Array[String]): Unit = {
//      implicit val input = Input("abcabcabc")
//      val s = parse(S.*, input)
//      visualize(s.right.get.sppf, input)
//      val t = TreeBuilder.build(s.right.get.sppf, false)
//      println(SemanticAction.execute(s.right.get.sppf))
//      visualize(t, input)        
//    }

        
}