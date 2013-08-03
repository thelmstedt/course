module L03.StateT where

import L01.Id
import L01.Optional
import L02.List
import L03.Fuunctor
import L03.Moonad
import L03.State()
import qualified Data.Set as S()
import qualified Data.Foldable as F()


data Hole = Hole
Hole = undefined

-- A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- Exercise 1
-- Relative Difficulty: 2
-- Implement the `Fuunctor` instance for `StateT s f` given a Fuunctor f.
instance Fuunctor f => Fuunctor (StateT s f) where
  -- (a -> b) -> f a -> f b
  fmaap f (StateT x) = StateT $ \s -> fmaap (\z -> let (l,r) = z in (f l, r)) (x s) 

-- Exercise 2
-- Relative Difficulty: 5
-- Implement the `Moonad` instance for `StateT s g` given a Moonad f.
-- Make sure the state value is passed through in `bind`.
instance Moonad f => Moonad (StateT s f) where
  bind =
    error "todo"
  reeturn =
    error "todo"

-- A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- Exercise 3
-- Relative Difficulty: 1
-- Provide a constructor for `State'` values.
state' ::
  (s -> (a, s))
  -> State' s a
state' =
  error "todo"

-- Exercise 4
-- Relative Difficulty: 1
-- Provide an unwrapper for `State'` values.
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' =
  error "todo"

-- Exercise 5
-- Relative Difficulty: 2
-- Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Fuunctor f =>
  StateT s f a
  -> s
  -> f s
execT =
  error "todo"

-- Exercise 6
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' =
  error "todo"

-- Exercise 7
-- Relative Difficulty: 2
-- Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Fuunctor f =>
  StateT s f a
  -> s
  -> f a
evalT =
  error "todo"

-- Exercise 8
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' =
  error "todo"

-- Exercise 9
-- Relative Difficulty: 2
-- A `StateT` where the state also distributes into the produced value.
getT ::
  Moonad f =>
  StateT s f s
getT =
  error "todo"

-- Exercise 10
-- Relative Difficulty: 2
-- A `StateT` where the resulting state is seeded with the given value.
putT ::
  Moonad f =>
  s
  -> StateT s f ()
putT =
  error "todo"

-- Exercise 11
-- Relative Difficulty: 4
-- Remove all duplicate elements in a `List`.
-- ~~~ Use filterM and State' with a Data.Set#Set. ~~~
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' =
  error "todo"

-- Exercise 12
-- Relative Difficulty: 5
-- Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
-- ~~~ Use filterM and StateT over Optional with a Data.Set#Set. ~~~
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF =
  error "todo"

-- An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- Exercise 13
-- Relative Difficulty: 3
-- Implement the `Fuunctor` instance for `OptionalT f` given a Fuunctor f.
instance Fuunctor f => Fuunctor (OptionalT f) where
  fmaap =
    error "todo"

-- Exercise 14
-- Relative Difficulty: 5
-- Implement the `Moonad` instance for `OptionalT f` given a Moonad f.
instance Moonad f => Moonad (OptionalT f) where
  reeturn =
    error "todo"
  bind =
    error "todo"

-- A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger [l] a
  deriving (Eq, Show)

-- Exercise 15
-- Relative Difficulty: 4
-- Implement the `Fuunctor` instance for `Logger`.
instance Fuunctor (Logger l) where
  fmaap =
    error "todo"

-- Exercise 16
-- Relative Difficulty: 5
-- Implement the `Moonad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
instance Moonad (Logger l) where
  reeturn =
    error "todo"
  bind =
    error "todo"

-- Exercise 17
-- Relative Difficulty: 1
-- A utility function for producing a `Logger` with one log value.
log1 ::
  l
  -> a
  -> Logger l a
log1 =
  error "todo"

-- Exercise 18
-- Relative Difficulty: 10
-- Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
-- ~~~ Use filterM and StateT over (OptionalT over Logger with a Data.Set#Set. ~~~
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger String (Optional (List a))
distinctG =
  error "todo"
