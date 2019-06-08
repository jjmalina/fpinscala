package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  } // 3

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => List()
      case Cons(head, tail) => tail
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def drop(i: Int, x: List[A]): List[A] = {
      if (i == 0) x
      l match {
        case Nil => List()
        case Cons(h, t) => drop(i - 1, t)
      }
    }
    drop(n, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => List()
      case Cons(h, t) => {
        if (f(h)) dropWhile(t, f)
        else l
      }
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => List()
    case Cons(h, Cons(h2, Nil)) => List(h)
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((a, b) => 1 + b)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(ll: List[A], acc: B): B = {
      ll match {
        case Nil => acc
        case Cons(x, Nil) => f(acc, x)
        case Cons(x, t) => loop(t, f(acc, x))
      }
    }
    loop(l, z)
  }

  def sum3(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product3(xs: List[Double]): Double = foldLeft(xs, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((acc, item) => Cons(item, acc))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil:List[A])(append)
  }

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((item, acc) => Cons(item + 1, acc))

  def stringifyList[A](l: List[A]): List[String] = {
    foldRight(l, Nil:List[String])((item, acc) => Cons(item.toString, acc))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((item, acc) => Cons(f(item), acc))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((item, acc) => {
      if (f(item)) Cons(item, acc)
      else acc
    })
  }

  // 20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
     concat(map(as)(f))
  }

  // 21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(item => {
      if (f(item)) {
        List(item)
      } else {
        Nil:List[A]
      }
    })
  }

  // 22
  def addCorresponding(l1: List[Int], l2: List[Int]): List[Int] = {
    l1 match {
      case Cons(head1, tail1) => {
        l2 match {
          case Cons(head2, tail2) => {
            Cons(head1 + head2, addCorresponding(tail1, tail2))
          }
          case Nil => Cons(head1, tail1)
        }
      }
      case Nil => {
        l2 match {
          case Cons(head2, tail2) => Cons(head2, tail2)
          case Nil => Nil
        }
      }
    }
  }

  // 23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  // 24
  def hasSubsequence[A](l: List[A], ll: List[A]): Boolean = {
    (l, ll) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) => {
        if (h1 == h2) {
          hasSubsequence(t1, t2)
        } else {
          hasSubsequence(t1, ll)
        }
      }
    }
  }
}
