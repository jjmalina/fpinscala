package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
  var cachedValue: Option[C] = None
  def isDone = cachedValue.isDefined
  def isCancelled = a.isCancelled || b.isCancelled
  def cancel(evenIfRunning: Boolean): Boolean =
    a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  def get = run(Long.MaxValue)
  def get(timeout: Long, units: TimeUnit): C = {
    val timeoutInNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
    run(timeoutInNanos)
  }

  def run(timeoutInNanos: Long): C = {
    cachedValue match {
      case Some(value) => value
      case None => {
        val start = System.nanoTime
        val aVal = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val elapsed = stop - start
        val bVal = b.get(timeoutInNanos - elapsed, TimeUnit.NANOSECONDS)
        val result = f(aVal, bVal)
        cachedValue = Some(result)
        result
      }
    }
  }
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map2Future[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def lazyUnit[A](a: => A): Par[A] = unit(a)

  def asyncF[A,B](f: A => B): A => Par[B] = (value: A) => lazyUnit(f(value))

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case head :: tail => map2(head, sequence(tail))(_ :: _)
      case _ => unit(List():List[A])
    }

  // We define `sequenceBalanced` using `IndexedSeq`, which provides an
  // efficient function for splitting the sequence in half.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequenceEfficient[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(
      sequenceEfficient(as.map(asyncF((a: A) => if (f(a)) List(a) else List())))
    )(_.flatten)

  def parSum(x: IndexedSeq[Int]): Par[Int] =
    fork {
      if (x.isEmpty) unit(0)
      else if (x.length == 1) unit(x.head)
      else {
        val (l, r) = x.splitAt(x.length / 2)
        map2(parSum(l), parSum(r))(_ + _)
      }
    }


  // this would be the generalization of sequencing?
  // sequence is normally implmenented in terms of foldright or foldleft
  def reduce[A](as: List[A])(f: (A, A) => A): Par[A] = ???

  /*
  7.7
  map(map(y)(g))(f) == map(y)(f compose g)
  map(map(y)(g))(f) == map(g(y))(f)
  map(g(y))(f) = f(g(y))
  f(g(y)) = f(g(y))
  */

  /*
  7.9
  Call fork n + 1 times where n is the size of the thread pool
  */
  def parMax(x: IndexedSeq[Int]): Par[Int] = ???

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => run(es)(choices(run(es)(n).get))
  }

  def choiceViaN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(value => if (value) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => run(es)(choices(run(es)(key).get))

  // aka flatMap
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => run(es)(choices(run(es)(pa).get))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => run(es)(f(run(es)(pa).get))

  def choiceMapViaChooser[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(choices)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices)

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(value => if (value) t else f)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val result = run(es)(a).get
      run(es)(result)
    }

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
