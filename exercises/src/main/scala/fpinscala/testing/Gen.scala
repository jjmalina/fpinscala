package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}
import scala.math

/*
8.1
  sum: List[Int] => Int
  Properties
    reversing a list and summing should be the same result
    sum of list of elements that are the same value should be length * value
    list of zeros is zero
    empty list is zero
    summing two partitioned lists and adding values is the same as summing the unpartitioned list
    1, 2, 3, ... n is equal to n*(n+1) / 2

8.2
  What properties specify a function that finds the maximum of a List[Int]
    max empty list is an exception?
    max of a single element list is the element
    max of an unsorted list is the last element of the sorted list
    max of a sorted list is the last element
    max of a descending sorted list is the first element
    max of a list is >= all elements
*/

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => {
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
    }
  }
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  }
  def tag(msg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, i) => Falsified(msg + "\n" + e, i)
      case x => x
    }
  }
}
object Prop {
  type MaxSize = Int
  type TestCases = Int
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  type FailedCase = String
  type SuccessCount = Int
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {
        (max, _, rng) => p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  val pint = Gen.choose(0, 10) map (Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  // 8.16
  val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20))
    .map(l => l.foldLeft(Par.unit(0))((parAcc, i) => Par.fork { Par.map2(parAcc, Par.unit(i))(_ + _) }))

  // 8.17
  val pint3 = choose(-100, 100) map (Par.unit(_))
  val checkFork = forAllPar(pint3)(x => equal(Par.fork(x), x))

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }


  // 8.18
  val lints = choose(0, 20).listOfN(choose(0, 20))
  val takeWhileProp1 = forAll(lints) { l => {
    val f = (x: Int) => true
    l.takeWhile(f) == l
  }}
  val takeWhileProp2 = forAll(lints) { l => {
    val f = (x: Int) => {
      val mid = l.size / 2
      x < l(mid)
    }
    l.takeWhile(f) ++ l.dropWhile(f) == l
  }}

  // 8.19
  def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
      val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    }
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))
  def unsized: SGen[A] = {
    SGen((i) => this)
  }
  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] =
    Gen(State(RNG.double).map(d => if (math.round(d) == 1) true else false))
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(value => if (value) g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Thresh = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if (d < g1Thresh) g1._1.sample else g2._1.sample))
  }
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen((n) => Gen.listOfN(n, g))
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen((n) => if (n == 0) Gen.listOfN(1, g) else Gen.listOfN(n, g))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}

/*
8.14 write a test suite to verify the behavior of List.sorted

val someInts = Gen.choose(-10, 10)
val sortedProp = Prop.forAll(Gen.listOf(someInts)) { l => {
  val lsorted = l.sorted
  if (l.length == 0) {
    lsorted == List()
  } else {
    !lsorted.zip(lsorted.tail).exists {case (a, b) => a > b} &&
    !lsorted.exists(!l.contains(_)) && !l.exists(!lsorted.contains(_))
  }
}}
*/