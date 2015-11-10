trait Part1_ListMaybe {
  import scala.StringContext
  type Int = scala.Int
    
  sealed abstract class List[T] {
    def ::(h: T): List[T] = new ::(h, this)
  }
  case class ::[T](h: T, t: List[T]) extends List[T] {
    override def toString = s"$h :: $t"
  }
  case class Nil[T]() extends List[T] {
    override def toString = "∅"
  }

  def ∅[T]: List[T] = Nil[T]()
    
  object List {
    def apply[T](vs: T*): List[T] = vs.foldRight(∅[T]){
      case (t, acc) => t :: acc
    }
  }
    
  // List(1,2,3) 
  // 1 :: 2 :: ∅

  implicit class RichInt(v: Int) {
    def to(end: Int): List[Int] = {
      def loop(xs: List[Int], i: Int): List[Int] = {
        if(i == 0) xs
        else loop(i :: xs, i - 1)
      }
      loop(Nil(), end)
    }
  }

  // 1 to 5

  sealed trait Maybe[T] {
    def |(v2: T): T = this match {
      case Just(v1) => v1
      case Empty() => v2
    }
  }
  case class Just[T](v: T) extends Maybe[T]
  case class Empty[T]() extends Maybe[T]
}