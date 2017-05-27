
object Main {

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








  // FP Stuff

  type String = java.lang.String

  trait Monoid[F] {
    def empty: F
    def append(a: F, b: => F): F
  }
    
  implicit class monoidSyntax[F](a: F)(implicit m: Monoid[F]){
    def |+|(b: => F): F = m.append(a, b)
  }
    
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class functorSyntax[F[_], A](fa: F[A])(implicit f: Functor[F]){
    def map[B](g: A => B): F[B] = f.map(fa)(g)
  }

  implicit def stringMoinoid: Monoid[String] = new Monoid  [String]{
    def empty: String = ""
    def append(a: String, b: => String): String = a + b
  }
    
  implicit def maybeMonoid[A](implicit ma: Monoid[A]): Monoid[Maybe[A]] = new Monoid[Maybe[A]]{
    def empty: Maybe[A] = Empty[A]()
    def append(a: Maybe[A], b: => Maybe[A]): Maybe[A] = {
      (a, b) match {
        case (Just(va), Just(vb)) => Just(ma.append(va, vb))
        case (Just(va), Empty()) => Just(ma.append(va, ma.empty))
        case (Empty(), Just(vb)) => Just(ma.append(ma.empty, vb))
        case (Empty(), Empty()) => Empty()
      }
    }
  }
    
  implicit def listFunctor: Functor[List] = new Functor[List]{
    def map[A, B](fa: List[A])(f: A => B): List[B] = {
      fa match {
        case h :: t => f(h) :: map(t)(f)
        case Nil() => Nil()
      }
    }
  }



  // Write a program that prints the numbers from 1 to 100.
  // But for multiples of three print “Fizz” instead of the
  // number and for the multiples of five print “Buzz”. For
  // numbers which are multiples of both three and five
  // print “FizzBuzz”.

  def mkString(xs: List[String], sep: String): String = {
    xs match {
      case Nil() => ""
      case h :: Nil() => h
      case h :: t => h + sep + mkString(t, sep)
    }
  }

  def w(v: Int, r: Int, s: String): Maybe[String] = 
    if(v % r == 0) Just(s)
    else Empty()
  
  val result =
    mkString((1 to 100).map(i =>
      ( w(i, 3, "fizz") |+|
        w(i, 5, "buzz")
      ) | i.toString
    ), java.lang.System.lineSeparator)



  def main(args: scala.Array[String]): scala.Unit = {
    scala.Console.println(result)
  }
}
