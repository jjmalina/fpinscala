package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Cons(h, t) => loop(t(), h() :: acc)
      }
    }
    loop(this, List():List[A]).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((a, b) => {
      if (p(a)) cons(a, b)
      else b
    })
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) =>
      if (f(a)) cons(a, b)
      else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => f(a).append(b))

  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this)(s => {
      s match {
        case Cons(h,t) => Some((f(h()), t()))
        case _ => None
      }
    })

  def takeUnfold(n: Int): Stream[A] =
    unfold((n, this))(state => {
      state._2 match {
        case Cons(h, t) if state._1 == 1 => Some((h(), (0, empty)))
        case Cons(h, t) if state._1 > 1 => Some((h(), (n - 1, t())))
        case _ => None
      }
    })

  def takeWhileUnfold(f: A => Boolean): Stream[A] =
    unfold(this)(stream => {
      stream match {
        case Cons(h, t) if f(h()) => Some((h(), t()))
        case _ => None
      }
    })

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => {
        Some(
          (Some(h1()), Some(h2())),
          (t1(), t2())
        )
      }
      case (Cons(h1, t1), Empty) => Some(
        (Some(h1()), Option.empty[B]), (t1(), Empty)
      )
      case (Empty, Cons(h2, t2)) => Some(
        (Option.empty[A], Some(h2())),
        (Empty, t2())
      )
      case _ => None
    }
  }

  /*
  official answer
  zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (h,h2) => h == h2
  }
  */
  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s) forAll {
      case (Some(a), Some(b)) => a == b
      case (None, Some(b)) => false
      case (Some(a), None) => true
      case _ => true
    }
  }


  // unfold(this) {
  //   case Empty => None
  //   case s => Some((s, s drop 1))
  // } append Stream(empty)
  def tails: Stream[Stream[A]] = {
    unfold(this)(s => {
      s match {
        case Cons(h, t) => Some((s, t()))
        case _ => None
      }
    })
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, b) => {
      val (acc, stream) = b
      lazy val result = f(a, acc)
      (result, cons(result, stream))
    })._2
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def fibs: Stream[Int] = {
    def loop(pprev: Int, prev: Int): Stream[Int] = {
      cons(pprev, loop(prev, pprev + prev))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((value, state)) => cons(value, unfold(state)(f))
      case None => empty
    }
  }

  def fibsUnfold: Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((n, n + 1)))

  def constantUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

  def onesUnfold: Stream[Int] = constantUnfold(1)
}