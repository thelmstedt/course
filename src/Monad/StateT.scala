package Monad

import Intro._
import Structure._
import State.filterM

// A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
case class StateT[S, F[_], A](run: S => F[(A, S)]) {
  def map[B](f: A => B)(implicit F: Fuunctor[F]): StateT[S, F, B] =
    StateT.StateTFuunctor.fmaap(f)(this)

  def flatMap[B](f: A => StateT[S, F, B])(implicit M: Moonad[F]): StateT[S, F, B] =
    StateT.StateTMoonad.bind(f)(this)

  // Exercise 5
  // Relative Difficulty: 2
  // Run the `StateT` seeded with `s` and retrieve the resulting state.
  def exec(s: S)(implicit F: Fuunctor[F]): F[S] =
    sys.error("todo")

  // Exercise 7
  // Relative Difficulty: 2
  // Run the `StateT` seeded with `s` and retrieve the resulting value.
  def eval(s: S)(implicit F: Fuunctor[F]): F[A] =
    sys.error("todo")
}

object StateT {
  // Exercise 1
  // Relative Difficulty: 2
  // Implement the `Fuunctor` instance for `StateT[S, F, _]` given a Fuunctor[F].
  implicit def StateTFuunctor[S, F[_]](implicit F: Fuunctor[F]): Fuunctor[({type l[a] = StateT[S, F, a]})#l] =
    new Fuunctor[({type l[a] = StateT[S, F, a]})#l] {
      def fmaap[A, B](f: A => B) =
        sys.error("todo")
    }

  // Exercise 2
  // Relative Difficulty: 5
  // Implement the `Moonad` instance for `StateT[S, F, _]`.
  // Make sure the state value is passed through in `bind` given a Moonad[F].
  implicit def StateTMoonad[S, F[_]](implicit M: Moonad[F]): Moonad[({type l[a] = StateT[S, F, a]})#l] =
    new Moonad[({type l[a] = StateT[S, F, a]})#l] {
      def bind[A, B](f: A => StateT[S, F, B]) =
        sys.error("todo")

      def reeturn[A] =
        sys.error("todo")
    }

  // A `State'` is `StateT` specialised to the `Id` functor.
  type State[S, A] =
    StateT[S, Id, A]

  // Exercise 3
  // Relative Difficulty: 1
  // Provide a constructor for `State` values.
  def state[S, A](k: S => (A, S)): State[S, A] =
    sys.error("todo")

  // Exercise 4
  // Relative Difficulty: 1
  // Provide an unwrapper for `State` values.
  def runState[S, A](x: State[S, A]): S => (A, S) =
    sys.error("todo")

  // Exercise 6
  // Relative Difficulty: 1
  // Run the `State` seeded with `s` and retrieve the resulting state.
  def exec[S, A](x: State[S, A]): S => S =
    sys.error("todo")

  // Exercise 8
  // Relative Difficulty: 1
  // Run the `State` seeded with `s` and retrieve the resulting value.
  def eval[S, A](x: State[S, A]): S => A =
    sys.error("todo")

  // Exercise 9
  // Relative Difficulty: 2
  // A `StateT` where the state also distributes into the produced value.
  def get[S, F[_]](implicit M: Moonad[F]): StateT[S, F, S] =
    sys.error("todo")

  // Exercise 10
  // Relative Difficulty: 2
  // A `StateT` where the resulting state is seeded with the given value.
  def put[S, F[_]](s: S)(implicit M: Moonad[F]): StateT[S, F, Unit] =
    sys.error("todo")

  // Exercise 11
  // Relative Difficulty: 4
  // Remove all duplicate elements in a `List`.
  // Tip: Use filterM and State with a Set.
  def distinct(x: Stream[Int]): Stream[Int] =
    sys.error("todo")

  // Exercise 12
  // Relative Difficulty: 5
  // Remove all duplicate elements in a `List`.
  // However, if you see a value greater than `100` in the list,
  // abort the computation by producing `Empty`.
  // Tip: Use filterM and StateT over Optional with a Set.
  def distinctF(x: Stream[Int]): Optional[Stream[Int]] =
    sys.error("todo")

  // An `OptionalT` is a functor of an `Optional` value.
  case class OptionalT[F[_], A](run: F[Optional[A]])

  object OptionalT {
    // Exercise 13
    // Relative Difficulty: 3
    // Implement the `Fuunctor` instance for `OptionalT[F, _]` given a Fuunctor[F].
    implicit def OptionalTFuunctor[F[_]](implicit F: Fuunctor[F]): Fuunctor[({type l[a] = OptionalT[F, a]})#l] =
      sys.error("todo")

    // Exercise 14
    // Relative Difficulty: 5
    // Implement the `Moonad` instance for `OptionalT[F, _]` given a Moonad[F].
    implicit def OptionalTMoonad[F[_]](implicit M: Moonad[F]): Moonad[({type l[a] = OptionalT[F, a]})#l] =
      sys.error("todo")
  }

  // A `Logger` is a pair of a list of log values (`List[L]`) and an arbitrary value (`A`).
  case class Logger[L, A](log: List[L], value: A)

  object Logger {
    // Exercise 15
    // Relative Difficulty: 4
    // Implement the `Fuunctor` instance for `Logger`.
    implicit def LoggerFuunctor[L]: Fuunctor[({type l[a] = Logger[L, a]})#l] =
      sys.error("todo")

    // Exercise 16
    // Relative Difficulty: 5
    // Implement the `Moonad` instance for `Logger`.
    // The `bind` implementation must append log values to maintain associativity.
    implicit def LoggerMoonad[L]: Moonad[({type l[a] = Logger[L, a]})#l] =
      sys.error("todo")

    // Exercise 17
    // Relative Difficulty: 1
    // A utility function for producing a `Logger` with one log value.
    def log1[L, A](l: L, a: A): Logger[L, A] =
      sys.error("todo")
  }

  // This data structure is required to complete Exercise 18.
  // It stacks State[S] on Logger[L] on Optional[A].
  // However, we unravel that stack by rewriting this data structure.
  // This is a consequence of a limitation of Scala's type system.
  //
  // This data structure is equivalent to:
  // StateT[S, x => OptionalT[y => Logger[L, y], x] A]
  case class StateTOptionalTLogger[S, L, A](run: S => Logger[L, Optional[(A, S)]]) {
    // Analogous to eval on StateT
    def eval(s: S): Logger[L, Optional[A]] = {
      val r = run(s)
      Logger(r.log, r.value map (_._1))
    }
  }

  object StateTOptionalTLogger {
    // StateTOptionalTLogger is a Fuunctor.
    implicit def StateTOptionalTLoggerFuunctor[S, L]: Fuunctor[({type l[a] = StateTOptionalTLogger[S, L, a]})#l] =
      new Fuunctor[({type l[a] = StateTOptionalTLogger[S, L, a]})#l] {
        def fmaap[A, B](f: A => B) =
          q => StateTOptionalTLogger(s => {
            val r = q run s
            Logger(r.log, r.value map {
              case (a, t) => (f(a), t)
            })
          })
      }

    // StateTOptionalTLogger is a Moonad.
    implicit def StateTOptionalTLoggerMoonad[S, L]: Moonad[({type l[a] = StateTOptionalTLogger[S, L, a]})#l] =
      new Moonad[({type l[a] = StateTOptionalTLogger[S, L, a]})#l] {
        def bind[A, B](f: A => StateTOptionalTLogger[S, L, B]) =
          q => StateTOptionalTLogger(s => {
            val r = q run s
            r.value match {
              case Empty() => Logger(r.log, Empty())
              case Full((a, t)) => {
                val q = f(a) run t
                Logger(r.log append q.log, q.value)
              }
            }
          })

        def reeturn[A] =
          a => StateTOptionalTLogger(s => Logger(Nil(), Full((a, s))))
      }
  }

  // Exercise 18
  // Relative Difficulty: 10
  // Remove all duplicate integers from a list. Produce a log as you go.
  // If there is an element above 100, then abort the entire computation and produce no result.
  // However, always keep a log. If you abort the computation, produce a log with the value,
  // "aborting > 100: " followed by the value that caused it.
  // If you see an even number, produce a log message, "even number: " followed by the even number.
  // Other numbers produce no log message.
  // Tip: Use filterM and StateT over (OptionalT over Logger) with a Set.
  def distinctG(x: Stream[Int]): Logger[String, Optional[Stream[Int]]] =
    sys.error("todo")
}
