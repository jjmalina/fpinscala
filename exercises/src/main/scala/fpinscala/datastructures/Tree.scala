package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // 25
  def size[A](t: Tree[A]): Int = {
    def loop(tree: Tree[A], acc: Int): Int = {
      tree match {
        case Leaf(value) => acc + 1
        case Branch(left, right) => 1 + loop(left, acc) + loop(right, acc)
      }
    }
    return loop(t, 0)
  }

  // 26
  def maximum(tree: Tree[Int]): Int = {
    def loop(t: Tree[Int], currentMax: Int): Int = {
      t match {
        case Leaf(value) => currentMax max value
        case Branch(left, right) => maximum(left) max maximum(right)
      }
    }
    loop(tree, 0)
  }

  // 27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(value) => 1
      case Branch(left, right) => (1 + depth(left)) max (1 + depth(right))
    }
  }

  // 28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // 29
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = {
    t match {
      case Leaf(value) => l(value)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }
  }

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)((item) => Leaf(f(item)): Tree[B])((l, r) => Branch(l, r))
  }

  def depthWithFold[A](tree: Tree[A]): Int = {
    fold(tree)((l) => 0)((d1, d2) => 1 + (d1 max d2))
  }

  def maximumWithFold(tree: Tree[Int]): Int = {
    fold(tree)((l) => l)((d1, d2) => d1 max d2)
  }

  def sizeWithFold[A](tree: Tree[A]): Int = {
    fold(tree)((l) => 1)(1 + _ + _)
  }
}