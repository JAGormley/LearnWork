package patmat

import common._
import patmat.Huffman.CodeTree
import patmat.Huffman.{Leaf, Fork}

object Tester extends App{

  def weight(tree: CodeTree): Int = tree match {
    case Fork(_,_,_,w) => w
    case Leaf(_,w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_,_,cs,_) => cs
    case Leaf(c,_) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))


  val sampleTree = makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2)
  )
  def unit(f: => Unit): String = {f; ""}
  unit(println("Will work"))



}

//object Scribble extends App {
//  def unit(f: => Unit): String = {f; ""}
//    unit(println("Will work"))
//}