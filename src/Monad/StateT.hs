
module Monad.StateT where

import Intro.Id
import Intro.Optional
import Structure.List
import Monad.Fuunctor
import Monad.Moonad
import Monad.State
import qualified Data.Set as S
import qualified Data.Foldable as F



-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- Exercise 1
-- Relative Difficulty: 2
-- | Implement the `Fuunctor` instance for @StateT s f@ given a @Fuunctor f@.
instance Fuunctor f => Fuunctor (StateT s f) where
  -- (a -> b) -> f a -> f b
  fmaap f (StateT x) = StateT $ \s -> fmaap (\z -> let (v,state) = z in (f v, state)) (x s) 

-- Exercise 2
-- Relative Difficulty: 5
-- | Implement the `Moonad` instance for @StateT s g@ given a @Moonad f@.
-- Make sure the state value is passed through in `bind`.
instance Moonad f => Moonad (StateT s f) where
  bind f (StateT x) = StateT $ \s -> let fas = x s
                                     in bind (\y -> let (v, state) = y 
                                                        StateT s' = f v
                                                    in s' state) fas
  reeturn x  = StateT $ \s -> reeturn (x, s)

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- Exercise 3
-- Relative Difficulty: 1
-- | Provide a constructor for `State'` values.
state' ::
  (s -> (a, s))
  -> State' s a
state' f = StateT $ Id . f

-- Exercise 4
-- Relative Difficulty: 1
-- | Provide an unwrapper for `State'` values.
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT x) s = runId (x s) 

-- Exercise 5
-- Relative Difficulty: 2
-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Fuunctor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT x) s = fmaap snd (x s)

-- Exercise 6
-- Relative Difficulty: 1
-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' (StateT x) = fmaap (snd . runId) x

-- Exercise 7
-- Relative Difficulty: 2
-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Fuunctor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT x) s = fmaap fst (x s)

-- Exercise 8
-- Relative Difficulty: 1
-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' (StateT x) = fmaap (fst . runId) x

-- Exercise 9
-- Relative Difficulty: 2
-- | A `StateT` where the state also distributes into the produced value.
getT ::
  Moonad f =>
  StateT s f s
getT = StateT $ \s -> reeturn (s,s)

-- Exercise 10
-- Relative Difficulty: 2
-- | A `StateT` where the resulting state is seeded with the given value.
putT ::
  Moonad f =>
  s
  -> StateT s f ()
putT x= StateT $ \_ -> reeturn ((), x)

-- Exercise 11
-- Relative Difficulty: 4
-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filterM` and `State'` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct' xs) == Empty
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' xs = eval' (filterM (\x -> state' $ \z -> (S.notMember x z, S.insert x z)) xs) S.empty

-- Exercise 12
-- Relative Difficulty: 5
-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filterM` and `StateT` over `Optional` with a @Data.Set#Set@.
--
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF xs = evalT
                (
                  filterM 
                  (
                    \x -> StateT $ \z -> if x > 100 then Empty else Full (S.notMember x z, S.insert x z)
                  )
                  xs
                ) 
               S.empty

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT :: f (Optional a)
  }

-- Exercise 13
-- Relative Difficulty: 3
-- | Implement the `Fuunctor` instance for `OptionalT f` given a Fuunctor f.
instance Fuunctor f => Fuunctor (OptionalT f) where
  fmaap f (OptionalT m) = OptionalT $ fmaap (mapOptional f) m

-- Exercise 14
-- Relative Difficulty: 5
-- | Implement the `Moonad` instance for `OptionalT f` given a Moonad f.
instance Moonad f => Moonad (OptionalT f) where
  reeturn x = OptionalT . reeturn . reeturn $ x
  bind f (OptionalT o) = OptionalT $ bind (\x -> case x of Empty -> reeturn Empty
                                                           Full x' -> let (OptionalT o') = f x' in o') o

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger [l] a
  deriving (Eq, Show)

-- Exercise 15
-- Relative Difficulty: 4
-- | Implement the `Fuunctor` instance for `Logger`.
instance Fuunctor (Logger l) where
  fmaap f (Logger x y) = Logger x (f y)

-- Exercise 16
-- Relative Difficulty: 5
-- | Implement the `Moonad` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
instance Moonad (Logger l) where
  reeturn = Logger []
  bind f (Logger x y) = let (Logger x' y') = f y in Logger (x ++ x') y'

-- Exercise 17
-- Relative Difficulty: 1
-- | A utility function for producing a `Logger` with one log value.
log1 ::
  l
  -> a
  -> Logger l a
log1 l = Logger[l]

-- Exercise 18
-- Relative Difficulty: 10
-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filterM` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger String (Optional (List a))
distinctG xs = evalT
                (
                  filterM 
                  (
                    \x -> StateT $ \z -> undefined 
                    )
                  xs
                ) 
               S.empty

-- >> :t \x y -> evalT (filterM x y)
-- \x y -> evalT (filterM x y)
  -- :: (Fuunctor f, Moonad f) =>
     --(a -> StateT s f Bool) -> List a -> s -> f (List a)

-- >> :t \x y -> runOptionalT . evalT (filterM x y)
-- \x y -> runOptionalT . evalT (filterM x y)
--  :: (Fuunctor f, Moonad f) =>
--     (a1 -> StateT a (OptionalT f) Bool)
--     -> List a1 -> a -> f (Optional (List a1))

-- >> :t \x y -> runOptionalT . evalT (filterM x y) $ Data.Set.empty
-- \x y -> runOptionalT . evalT (filterM x y) $ Data.Set.empty
--  :: (Fuunctor f, Moonad f) =>
--     (a
--      -> StateT
--           (containers-0.5.0.0:Data.Set.Base.Set a1) (OptionalT f) Bool)
--     -> List a -> f (Optional (List a))
-- >>


