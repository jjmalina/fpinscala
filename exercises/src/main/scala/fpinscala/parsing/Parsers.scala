package fpinscala.parsing

import language.higherKinds
import java.util.regex._
import scala.util.matching.Regex

import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  // Recognizes and returns a single String
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString).map(d => d.charAt(0))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(v => succeed(f(v)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2).map (f.tupled)

  // Always succeeds with the value a
  def succeed[A](a: A): Parser[A] = string("") map ( _ => a)

  // Returns the portion of input inspected by p if successful
  def slice[A](p: Parser[A]): Parser[String]

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    def loop(i: Int, acc: Parser[List[A]]): Parser[List[A]] =
      if (i == n) acc else loop(i + 1, map2(p, acc)(_ :: _))
    loop(1, succeed(List()))
  }

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def attempt[A](p: Parser[A]): Parser[A]

  /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
  // def furthest[A](p: Parser[A]): Parser[A]

  /** In the event of an error, returns the error that occurred most recently. */
  // def latest[A](p: Parser[A]): Parser[A]

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] = skipR(attempt(p), whitespace)

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(skipL(p2, p)))(_ :: _)

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] = regex("\\z".r)

  def root[A](p: Parser[A]): Parser[A] =
    skipR(p, eof)

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    skipR(skipL(start, p), stop)

  def repeatedLetters: Parser[List[Char]] = {
    /* "0" or "1a"  or "2aa", etc...
      Alternatively
      for {
        digit <- "[0-9]+".r
        val n = digit.toInt
        _ <- listOfN(n, char('a'))
      } yield n
    */
    regex("^[0-9].*".r).flatMap(s => listOfN(s.substring(0, 1).toInt, char('a')))
  }

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = skipL(string("\""), thru("\"").map(_.dropRight(1)))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] = token(quoted)

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] = token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString.map(_.toDouble)

  def productFmap[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def map2Fmap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for { a <- p; b <- p2 } yield f(a,b)

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many1: Parser[List[A]] = self.many1(p)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
  }

  // val numA: Parser[Int] = char('a').many.map(_.size)
  val ab = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)

    def productLaw[A](a: Parser[A], b: Parser[A], c: Parser[A])(in: Gen[String]): Prop =
      equal((a ** b) ** c map(unbiasL), a ** (b ** c) map(unbiasR))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

object Parsers {}
case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {

  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location,String)] =
    stack.lastOption
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  /* I have to admit that most of this is borrowed from the answer given that
   writing a parser from an abstract library of combinators (some of which aren't
   even implemented) is very difficult when doing it for the first time :/
  */
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def array: Parser[JSON] =
      surround("[","]")(sep(value, char(','))).map(a => JArray(a.toIndexedSeq))
    def obj: Parser[JSON] =
      surround("{", "}")(
        sep(escapedQuoted ** skipL(char(':'), value), char(','))
      ).map(kvs => JObject(kvs.toMap))
    def lit: Parser[JSON] = {
      "null".as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array

    root(skipL(whitespace, obj | array))
  }
}

object Types {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def extract: Either[ParseError,A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e,c) => Failure(f(e), c)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case  _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}
import Types._

object MyParsers extends Parsers[Parser] {
  def run[A](p: Parser[A])(s: String): Either[ParseError,A] = {
    val s0 = Location(s)
    p(s0).extract
  }

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    s => x(s) match {
      case Failure(e, false) => y(s)
      case r => r
    }

  def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a, n) => g(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
      case e@Failure(_,_) => e
    }

  def string(s: String): Parser[String] = {
    (l) =>
      if (l.input.startsWith(s))
        Success(s, l.offset)
      else
        Failure(l.toError("Expected: " + s), true)
  }

  def regex(r: Regex): Parser[String] = {
    val msg = "Regex: " + r
    (l) => r.findPrefixOf(l.input) match {
      case Some(m) => Success(m, m.length)
      case None => Failure(Location(l.input, l.offset).toError(msg), false)
    }
  }

  override def succeed[A](a: A): Parser[A] = (l) => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    (l) => p(l) match {
      case Success(ps, i) => Success(l.input.slice(0,i), i)
      case f@Failure(_, _) => f
    }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.push(l, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.label(msg))
}


/**
 * JSON parsing example.
 */
object JSONExample extends App {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  val P = MyParsers

  def printResult[E](e: Either[E,JSON]) =
    e.fold(println, println)

  val json: Parser[JSON] = JSON.jsonParser(P)
  printResult { P.run(json)(jsonTxt) }
  println("--")
  printResult { P.run(json)(malformedJson1) }
  println("--")
  printResult { P.run(json)(malformedJson2) }
}
