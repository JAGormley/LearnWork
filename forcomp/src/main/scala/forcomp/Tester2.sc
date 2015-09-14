//import common._

object tester {

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys);
  }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x*x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 =>
      val (first, rest) = xs span(y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (x => (x.head, x.length))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( ??? )

  squareList(List(2, 4, 2))
  squareList2(List(2, 4, 2))
  pack(List("a", "a", "a", "b", "c", "c", "a"))
  encode(List("a", "a", "a", "b", "c", "c", "a"))
}

