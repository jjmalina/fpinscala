package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 0
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 match {
      case Some(aa) => a1
      case None => a2
    }
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = (a: A) => a2(a1(a))
    def zero() = (a: A) => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

// Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val abc = gen.map2(gen)((a, b) => (a, b)).map2(gen)((a, b) => (a._1, a._2, b))
    forAll(abc) { ((a: A, b: A, c: A) => {
      m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
    }).tupled} && forAll((gen)) { x => m.op(m.zero, x) == x}
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b: B, a: A) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 1) {
      as.foldLeft(m.zero)((b: B, a: A) => m.op(b, f(a)))
    } else {
      val mid = as.length / 2
      m.op(
        foldMapV(as.slice(0, mid), m)(f),
        foldMapV(as.slice(mid, as.length), m)(f)
      )
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMap(ints.toList, new Monoid[(Int, Boolean)] {
      def zero = (-Int.MaxValue, true)
      def op(a: Tuple2[Int, Boolean], b: Tuple2[Int, Boolean]): Tuple2[Int, Boolean] = {
        val (aI, aV) = a
        val (bI, bV) = b
        if (!aV) {
          (bI, false)
        } else {
          (bI, aI <= bI)
        }
      }
    })((a: Int) => (a, true))._2

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op(a: Par[A], b: Par[A]) = a.map2(b)(m.op)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM = par(m)
    if (v.length == 1) {
      v.foldLeft(parM.zero)((b: Par[B], a: A) => parM.op(b, Par.unit(f(a))))
    } else {
      val mid = v.length / 2
      parM.op(
        parFoldMap(v.slice(0, mid), m)(f),
        parFoldMap(v.slice(mid, v.length), m)(f)
      )
    }
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    val zero = Stub("")

    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    def toWC(c: Char): WC = {
      if (c.isWhitespace) {
        Part("", 0, "")
      } else
        Stub(c.toString)
    }
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(toWC) match {
      case Stub(s) => unstub(s)
      case Part(lStub, words, rStub) => unstub(lStub) + words + unstub(rStub)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      val zero = (A.zero, B.zero)
      def op(left: (A, B), right: (A, B)): (A, B) = {
        val (a1, b1) = left
        val (a2, b2) = right
        (A.op(a1, a2), B.op(b1, b2))
      }
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def zero = (a: A) => B.zero
      def op(l: A => B, r: A => B): A => B = {
        (a: A) => {
          B.op(l(a), r(a))
        }
      }
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(
            k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero))
          )
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val mmM: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    IndexedSeqFoldable.foldMap(as)((a: A) => Map(a -> 1))(mmM)
  }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as match {
      case head :: tl => f(head, foldRight(tl)(z)(f))
      case Nil => z
    }
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as match {
      case head :: tl => foldLeft(tl)(f(z, head))(f)
      case Nil => z
    }
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b: B, a: A) => mb.op(f(a), b))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    if (as.length == 0) {
      z
    } else {
      f(as.head, foldRight(as.tail)(z)(f))
    }
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    if (as.length == 0) {
      z
    } else {
      foldLeft(as.tail)(f(z, as.head))(f)
    }
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b: B, a: A) => mb.op(f(a), b))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    if (as.isEmpty) {
      z
    } else {
      f(as.head, foldRight(as.tail)(z)(f))
    }
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    if (as.isEmpty) {
      z
    } else {
      foldLeft(as.tail)(f(z, as.head))(f)
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      case Leaf(value) => f(value)
    }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
      case Leaf(value) => f(z, value)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      case Leaf(value) => f(value, z)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b: B, a: A) => mb.op(b, f(a)))
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case Some(value) => f(z, value)
      case None => z
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as match {
      case Some(value) => f(value, z)
      case None => z
    }
}
