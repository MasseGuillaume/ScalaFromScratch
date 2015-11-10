// Write a program that prints the numbers from 1 to 100.
// But for multiples of three print “Fizz” instead of the
// number and for the multiples of five print “Buzz”. For
// numbers which are multiples of both three and five
// print “FizzBuzz”.
trait Part3_FizzBuzz extends Part2_FunctionalProgramming {

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
}