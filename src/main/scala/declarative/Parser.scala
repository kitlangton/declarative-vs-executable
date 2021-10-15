package declarative

import scala.collection.mutable

final case class Coord(x: Int, y: Int)

// flatMap == run this thing, then use its result to run that thing

sealed trait Pipeline[-In, +Out] {
  def >>>[Out2](that: Pipeline[Out, Out2]) : Pipeline[In, Out2] = ???
}

object Pipeline {

}

// DECLARATIVE
// - defined as a sealed trait
sealed trait Parser[+A] { self =>

  // - we write an in
  // interpreter for our description/DSL
  def parse[T](input: String): Either[String, A] = {
    var remainder                                = input
    var currentParser: Parser[Any]               = self.asInstanceOf[Parser[Any]]
    val stack: mutable.Stack[Any => Parser[Any]] = mutable.Stack[Any => Parser[Any]]()
    var loop                                     = true
    var result: Either[String, A]                        = Left("OOPS")
    // orElse
    // descriptive error
    // tracking the input position

    def done(optionA: Either[String, Any]): Unit =
      optionA match {
        case Right(a) =>
          if (stack.isEmpty) {
            loop = false
            result = optionA.asInstanceOf[Either[String, A]]
          } else {
            val cont = stack.pop()
            currentParser = cont(a)
          }
        case Left(message) =>
          loop = false
          result = Left(message)
      }

    while (loop)
//      println(s"""
//REMAINDER: $remainder
//CURRENT PARSER: $currentParser
//STACK: $stack
//LOOP: $loop
//RESULT: $result
//         """)
      currentParser match {
        case Parser.Succeed(a) =>
          done(Right(a))

        case Parser.Fail(message) =>
          done(Left(message))

        case Parser.AnyChar =>
          remainder.headOption match {
            case Some(c) =>
              remainder = remainder.tail
              done(Right(c))
            case _ =>
              done(Left("INPUT WAS EMPTY"))
          }

        case Parser.CharParser(char) =>
          remainder.headOption match {
            case Some(head) if head == char =>
              remainder = remainder.tail
              done(Right(char.asInstanceOf[A]))
            case _ =>
              done(Left("INPUT WAS EMPTY"))
          }

        case Parser.StringParser(string) =>
          if (remainder.startsWith(string)) {
            remainder = remainder.drop(string.length)
            done(Right(string.asInstanceOf[A]))
          } else
            done(Left(s"did not match string! $string"))

        case Parser.FlatMap(parser, f) =>
          currentParser = parser
          stack.push(f.asInstanceOf[Any => Parser[Any]])
      }

    result
  }

  //  def separatedBy(parser: Parser[Any]): Parser[scala.List[A]] = ???

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser.FlatMap(self, f)

  def map[B](f: A => B): Parser[B] =
    flatMap(a => Parser.succeed(f(a)))
}

object Parser {
  def string(str: String): Parser[String] =
    StringParser(str)

  def char(c: Char): Parser[Char] =
    CharParser(c)

  def anyChar: Parser[Char] =
    AnyChar

  def fail(message: String): Parser[Nothing] = Fail(message)

  val int: Parser[Int] =
    anyChar.flatMap { char =>
      char.toString.toIntOption match {
        case Some(int) => succeed(int)
        case None      => fail(s"expected int, found $char!")
      }
    }

  def succeed[A](a: A): Parser[A] =
    Succeed(a)

  final case class Succeed[+A](a: A) extends Parser[A]
  final case class Fail(message: String) extends Parser[Nothing]
  case object AnyChar extends Parser[Char]
  case object IntParser extends Parser[Int]
  final case class CharParser(char: Char) extends Parser[Char]
  final case class OrElse[A](lhs: Parser[A], rhs: Parser[A]) extends Parser[A]
  final case class StringParser(string: String) extends Parser[String]
  final case class FlatMap[A, B](parser: Parser[A], f: A => Parser[B]) extends Parser[B]
}

object ParserExamples {
  val intParser: Parser[Int] = Parser.int

  val coordParser: Parser[Coord] =
    for {
      _ <- Parser.char('(')
      x <- Parser.int
      _ <- Parser.string(", ")
      y <- Parser.int
      _ <- Parser.char(')')
    } yield Coord(x, y)

  def main(args: Array[String]): Unit =
    println(coordParser.parse("(8, x)"))
}

object Example {

  trait Associative[A] {
    def combine(left: A, right: A): A
  }

  object Associative {
    val intAssociative = new Associative[Int] {
      override def combine(left: Int, right: Int): Int =
        left + right
    }
  }
}

// You can interpret a declarative encoding in different ways
object MyCustomParser {
  def parseCustom[A](parser: Parser[A], string: String): Either[String, A] =
    parseImpl(parser, string).map(_._1)

  def execute[A](parser: Parser[A]): executable.Parser[A] =
    parser match {
      case Parser.Succeed(a) => executable.Parser.succeed(a)
      case Parser.Fail(message) => ???
      case Parser.AnyChar => ???
      case Parser.IntParser => ???
      case Parser.CharParser(char) => ???
      case Parser.OrElse(lhs, rhs) => ???
      case Parser.StringParser(string) => ???
      case Parser.FlatMap(parser, f) => ???
    }

  def parseImpl[A](parser: Parser[A], string: String): Either[String, (A, String)] =
    parser match {
      case Parser.Succeed(a)           => ???
      case Parser.AnyChar              => ???
      case Parser.IntParser            => ???
      case Parser.CharParser(char)     => ???
      case Parser.StringParser(string) => ???
      case Parser.FlatMap(parser, f)   =>
        parseImpl(parser, string) match {
          case Right((value, remainder)) =>
            parseImpl(f.asInstanceOf[Any => Parser[A]](value), remainder)
          case Left(message) =>
            Left(message)
        }
    }
}
