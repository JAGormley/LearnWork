//package patmat
//import common._
import patmat.Huffman.CodeTree
import patmat.Huffman.{Leaf, Fork}
object Yep {

  def times(chars: List[Char]): List[(Char, Int)] = timesHelper(chars, List())
  def timesHelper(chars: List[Char], acc: List[(Char, Int)]) : List[(Char, Int)] = {
    if (chars.isEmpty){
      return acc
    }
    timesHelper(chars.tail, timesHelper_(chars.head, acc))
  }
  def timesHelper_(c: Char, list: List[(Char, Int)]) : List[(Char, Int)] = {
    if (list.isEmpty) {
      return List((c, 1))
    }
    if (list.head._1 == c){
      return (list.head._1, list.head._2+1)::list.tail
    }
    else list.head :: timesHelper_(c, list.tail)
  }

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
  times(List('b', 'c',  'b', 'b'))
}
