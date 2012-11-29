import Prelude hiding (head, tail, zip, unzip)
import Data.Foldable
import Data.Traversable
import Control.Arrow

data Free f a =
  Unit a
  | Free (f (Free f a))

instance Functor f =>
  Functor (Free f) where
  fmap f (Unit a) =
    Unit (f a)
  fmap f (Free k) = 
    Free (fmap (fmap f) k)

instance Functor f =>
  Monad (Free f) where
  return =
    Unit
  Unit a >>= f =
    f a
  Free k >>= f =
    Free (fmap (>>= f) k)

instance (Functor f, Foldable f) =>
  Foldable (Free f) where
  foldMap f (Unit a) =  
    f a
  foldMap f (Free k) =
    fold (fmap (foldMap f) k)

instance Traversable f =>
   Traversable (Free f) where
  traverse f (Unit a) =
    fmap Unit (f a)
  traverse f (Free k) =
    fmap Free (traverse (traverse f) k)

type List a =
  Free ((,) a) ()

data Trivial a =
  Trivial

type Boolean =
  Free Trivial ()

false ::
  Boolean
false =
  Free Trivial

true ::
  Boolean
true =
  Unit ()

not ::
  Boolean
  -> Boolean
not (Free _) =
  Unit ()
not (Unit _) =
  Free Trivial

type Option a =
  Free Trivial a

none ::
  Option a
none =
  Free Trivial

some ::
  a
  -> Option a
some =
  Unit

fromOption ::
  a
  -> Option a
  -> a
fromOption a (Free _) =
  a
fromOption _ (Unit a) =
  a

isNone ::
  Option a
  -> Boolean
isNone (Free _) =
  true
isNone (Unit _) =
  false

isSome ::
  Option a
  -> Boolean
isSome (Free _) =
  false
isSome (Unit _) =
  true
  
type Iteratee t a =
  Free ((->) t) a

data StateF s a =
  Get (s -> a)
  | Set s a

type State s a =
  Free (StateF s) a

data Cofree f a =
  Cofree a (f (Cofree f a))

instance Functor f =>
  Functor (Cofree f) where
  fmap f (Cofree h t) = 
    Cofree (f h) (fmap (fmap f) t)

class Functor f =>
  Comonad f where
  counit ::
    f a
    -> a
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

instance Functor f => 
  Comonad (Cofree f) where
  counit (Cofree h _) =
    h
  f <<= a@(Cofree h t) =    
    Cofree (f a) (fmap (f <<=) t)

newtype Id a =
  Id a

type Stream a =
  Cofree Id a

type NonEmptyList a =
  Cofree Maybe a

type Tree a =
  Cofree [] a

head ::
  NonEmptyList a
  -> a
head (Cofree h _) =
  h

tail ::
  NonEmptyList a
  -> Maybe (NonEmptyList a)
tail (Cofree _ t) =
  t

single ::
  a
  -> NonEmptyList a
single a =
  Cofree a Nothing

cons ::
  a
  -> NonEmptyList a
  -> NonEmptyList a
cons a =
  Cofree a . Just

foldrN ::
  (a -> b -> b)
  -> b
  -> NonEmptyList a
  -> b
foldrN f b (Cofree h Nothing) =
  f h b
foldrN f b (Cofree h (Just t)) =
  f h (foldrN f b t)

append ::
  NonEmptyList a
  -> NonEmptyList a
  -> NonEmptyList a  
append =
  flip (foldrN cons)

bind ::
  (a -> NonEmptyList b)
  -> NonEmptyList a
  -> NonEmptyList b
bind f (Cofree h Nothing) =
  f h
bind f (Cofree h (Just t)) =
  append (f h) (bind f t)

zip ::
  NonEmptyList a
  -> NonEmptyList b
  -> NonEmptyList (a, b)
zip x y =
  bind (\a -> fmap ((,) a) y) x

unzip ::
  NonEmptyList (a, b)
  -> (NonEmptyList a, NonEmptyList b)
unzip (Cofree (a, b) Nothing) = 
  (single a, single b)
unzip (Cofree (a, b) (Just t)) =
  let (as, bs) = unzip t
  in (cons a as, cons b bs)
