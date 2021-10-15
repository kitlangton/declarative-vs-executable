package executable

// Hello!
// Zymposium!
//
// Techniques for designing programs and domain specific languages
// EXECUTABLE VS DECLARATIVE ENCODINGS
// - executable is DOING IT LIVE
// - declarative is BUILDING A PLAN (as a value)
//
// PARSING PARSER
// "(10, 2)"  -> Some(Coord(10, 2))
// "nonsense" -> None
final case class Coord(x: Int, y: Int)

// flatMap == run this thing, then use its result to run that thing

trait Parser[+A] { self =>
  def parseAll(input: String): Option[A] =
    parse(input).map(_._1)

  def parse(input: String): Option[(A, String)]

  def separatedBy(parser: Parser[Any]): Parser[scala.List[A]] = ???

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    new Parser[B] {
      override def parse(input: String): Option[(B, String)] = {
        self.parse(input) match {
          case Some((a, remainder)) =>
            f(a).parse(remainder)
          case None => None
        }
      }
    }

  def map[B](f: A => B): Parser[B] =
    flatMap(a => Parser.succeed(f(a)))
}

object Parser {
  def string(str: String): Parser[String] =
    new Parser[String] {
      override def parse(input: String): Option[(String, String)] =
        if (input.startsWith(str)) Some((str, input.drop(str.length)))
        else None
    }

  def char(c: Char): Parser[Char] =
    new Parser[Char] {
      override def parse(input: String): Option[(Char, String)] =
        input.headOption match {
          case Some(head) if head == c => Some((c, input.tail))
          case _ => None
        }
    }

  def anyChar: Parser[Char] =
    new Parser[Char] {
      override def parse(input: String): Option[(Char, String)] =
        input.headOption match {
          case Some(c)  => Some((c, input.tail))
          case _ => None
        }
    }

  // 1 8 9
  // 123
  val int: Parser[Int] =
    anyChar.flatMap { char =>
      new Parser[Int]{
        override def parse(input: String): Option[(Int, String)] =
          char.toString.toIntOption match {
            case Some(int) => Some((int, input))
            case None => None
          }
      }
    }

  def succeed[A](a: A): Parser[A] =
    new Parser[A] {
      override def parse(input: String): Option[(A, String)] =
        Some((a, input))
    }
}


object ParserExamples {
  val intParser: Parser[Int] = Parser.int

  val coordParser : Parser[Coord] =
    for {
      _ <- Parser.char('(')
      x <- Parser.int
      _ <- Parser.string(", ")
      y <- Parser.int
      _ <- Parser.char(')')
    } yield Coord(x,y)

  def main(args: Array[String]): Unit = {
    println(coordParser.parse("(8, 3)"))
  }
}
