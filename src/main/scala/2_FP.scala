trait Part2_FunctionalProgramming extends Part1_ListMaybe {
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
}