package quiz

import scala.annotation.tailrec

case class Token(north: Int, south: Int)
object Domino {

  def apply(n: Int): List[Token] = {
    @tailrec
    def loop(n: Int, acc: List[Token]): List[Token] = {
      if (n <= 1) acc
      else {
        val cycles: List[Token] = (for (i <- 2 to n - 1) yield (List(Token(n, i), Token(i, n)))).flatten.toList
        val subseq: List[Token] = Token(1, n) :: cycles ::: Token(n, 1) :: Nil
        loop(n-1, subseq ::: acc)
      }
    }
    loop(n, List())
  }

  def main(args: Array[String]): Unit = {
    (1 to 5).foreach {
      i =>
        val ds = Domino(i)
        println(ds)
        assert(validate(ds))
        assert(ds.size == ds.toSet.size)
        assert(ds.size == i * (i - 1))
    }
  }

  def validate(ds: Seq[Token]): Boolean = {
    ds.sliding(2, 1).forall { x =>
      x match {
        case List(left, right) => left.south == right.north
      }
    }
  }
}