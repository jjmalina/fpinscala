package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    (if (int < 0) -(int + 1) else int, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 1) {
      val (i, r) = nonNegativeInt(rng)
      (List(i), r)
    } else {
      val (i, r) = nonNegativeInt(rng)
      val (l, r2) = ints(count - 1)(r)
      (i :: l, r2)
    }
  }

  def doubleViaMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r) = ra(rng)
      val (b, r2) = rb(r)
      (f(a, b), r2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List[A](), rng)) {
        case (fn, (acc, r)) => {
          val (a, r2) = fn(r)
          (a :: acc, r2)
        }
      }
    }
  }

  def intsSequence(count: Int)(rng: RNG): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })
  }

  def mapViaFlatmap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def modify[S](f: S => S): State[S, Unit] = for {
    // get.flatMap(s => set(f(s)).flatMap(_))
    s <- StateUtil.get
    _ <- StateUtil.set(f(s))
  } yield ()

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    State(initial => {
      fs.foldRight((List[A](), initial)) {
        case (State(fn), (l, s)) => {
          val (next, s2) = fn(s)
          (next :: l, s2)
        }
      }
    })

  def sequenceViaFoldLeft[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List()))((acc, s) => s.map2(acc)((a, b) => a :: b))
}

object StateUtil {
  def get[S]: State[S,S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Candy {
  def applyInput(input: Input): Machine => Machine = {
    (machine) => {
      (input, machine) match {
        case (Coin, Machine(locked, candies, coins)) if locked && candies > 0 => {
          Machine(false, candies, coins + 1)
        }
        case (Turn, Machine(locked, candies, coins)) if !locked && candies > 0 => {
          Machine(true, candies - 1, coins)
        }
        case (_, m) => m
      }
    }
  }
  // https://groups.google.com/d/msg/scala-functional/8iHaSFPPjgw/N7Scz7yBz9IJ
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val ss: State[Machine, List[Unit]] = State.sequence(
      inputs.map(input => State.modify(applyInput(input)))
    )
    /*
    State(s => {
      val (a, s2) = run(s)
      (
        State(s => (s.coins, s.candies))
      ).run(s2)
    })
    */
    ss.flatMap(_ => StateUtil.get.map(s => (s.coins, s.candies)))
  }
}