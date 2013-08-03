package Monad

import Intro._
import Structure._
import math.BigInt

// A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State.StateFuunctor.fmaap(f)(this)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State.StateMoonad.bind(f)(this)

  // Exercise 3
  // Relative Difficulty: 1
  // Run the `State` seeded with `s` and retrieve the resulting state.
  def exec(s: S): S =
    sys.error("todo")

  // Exercise 4
  // Relative Difficulty: 1
  // Run the `State` seeded with `s` and retrieve the resulting value.
  def eval(s: S): A =
    sys.error("todo")

}

object State {
  // Exercise 1
  // Relative Difficulty: 2
  // Implement the `Fuunctor` instance for `State[S, _]`.
  implicit def StateFuunctor[S]: Fuunctor[({type l[a] = State[S, a]})#l] =
    new Fuunctor[({type l[a] = State[S, a]})#l] {
      def fmaap[A, B](f: A => B) =
        sys.error("todo")
    }

  // Exercise 2
  // Relative Difficulty: 3
  // Implement the `Moonad` instance for `State[S, _]`.
  // Make sure the state value is passed through in `bind`.
  implicit def StateMoonad[S]: Moonad[({type l[a] = State[S, a]})#l] =
    new Moonad[({type l[a] = State[S, a]})#l] {
      def bind[A, B](f: A => State[S, B]) =
        sys.error("todo")

      def reeturn[A] =
        sys.error("todo")
    }

  // Exercise 5
  // Relative Difficulty: 2
  // A `State` where the state also distributes into the produced value.
  def get[S]: State[S, S] =
    sys.error("todo")

  // Exercise 6
  // Relative Difficulty: 2
  // A `State` where the resulting state is seeded with the given value.
  def put[S](s: S): State[S, Unit] =
    sys.error("todo")

  // Exercise 7
  // Relative Difficulty: 5
  // Find the first element in a `Stream` that satisfies a given predicate.
  // It is possible that no element is found, hence an `Optional` result.
  // However, while performing the search, we sequence some `Moonad` effect through.
  //
  // Note the similarity of the type signature to Stream#find
  // where the effect appears in every return position:
  //   find ::  (A =>   Bool ) => Stream[A] ->   Optional[A]
  //   findM :: (A => F[Bool]) => Stream[A] -> F[Optional[A]]
  def findM[F[_], A](p: A => F[Boolean], x: Stream[A])(implicit M: Moonad[F]): F[Optional[A]] =
    sys.error("todo")

  // Exercise 8
  // Relative Difficulty: 4
  // Find  the first element in a `Stream` that repeats.
  // It is possible that no element repeats, hence an `Optional` result.
  // Tip: Use findM and State with a Set.
  def firstRepeat[A](x: Stream[A]): Optional[A] =
    sys.error("todo")

  // Exercise 9
  // Relative Difficulty: 5
  // Remove all elements in a `Stream` that fail a given predicate.
  // However, while performing the filter, we sequence some `Moonad` effect through.
  //
  // Note the similarity of the type signature to Stream#filter
  // where the effect appears in every return position:
  //   filter ::  (A =>   Bool ) => Stream[A] =>   Stream[A]
  //   filterM :: (A => F[Bool]) => Stream[A] => F[Stream[A]]
  def filterM[F[_], A](p: A => F[Boolean], x: Stream[A])(implicit M: Moonad[F]): F[Stream[A]] =
    sys.error("todo")

  // Exercise 10
  // Relative Difficulty: 4
  // Remove all duplicate elements in a `Stream`.
  // // Tip: Use filterM and State with a Set.
  def distinct[A](x: Stream[A]): Stream[A] =
    sys.error("todo")

  // Exercise 11
  // Relative Difficulty: 3
  // Produce an infinite `Stream` that seeds with the given value at its head,
  // then runs the given function for subsequent elements
  def produce[A](f: A => A, a: A): Stream[A] =
    sys.error("todo")

  // Exercise 12
  // Relative Difficulty: 10
  // A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
  // In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
  // because it results in a recurring sequence.
  // Tip: Use findM with State and produce
  // Tip: Use flaatten to write a square function
  // Tip: Use library functions: containsOptional (below)
  def isHappy(i: BigInt): Boolean =
    sys.error("todo")

  ///////////////////////
  // SUPPORT LIBRARIES //
  ///////////////////////

  def containsOptional[A](a: A)(o: Optional[A]): Boolean =
    o match {
      case Empty() => false
      case Full(x) => a == x
    }
}
