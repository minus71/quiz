package quiz

case class Token(north: Int, south: Int)
object Domino {
  
  def apply(n: Int): Seq[Token] = {
    if (n <= 1) Nil
    else {
      val pairs = (1 until n).map(x=>(Token(x, n), Token(n, x)))
      insert(pairs, apply(n - 1))
    }
  }
 
  def insert(left: Token, right: Token, tokenSequence: Seq[Token]): Seq[Token] = {
    tokenSequence.span { token => token.north != right.south } match {
      case (lefts, Nil)              => lefts :+ left :+ right
      case (lefts, rights) => lefts ++ (left +: right +: rights)
    }
  }
  
 
  def insert(pairs: Seq[(Token, Token)], tokenSequence: Seq[Token]): Seq[Token] = {
    pairs match {
      case (left, right) +: tail => {
        def newSeq = insert(left, right, tokenSequence)
        insert(tail, newSeq)
      }
      case Nil => tokenSequence
    }
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