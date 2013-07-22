module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  fmaap' :: (a -> b) -> m a -> m b
  fmaap' f = bind (reeturn . f)

-- Exercise 5
-- Relative Difficulty: 1
instance Moonad Id where
  -- (a -> Id b) -> Id a -> Id b
  -- once inhabited, theorems for free
  -- intuitionist logic
  -- types as theorems, programs as proofs
  bind = flip bindId
  reeturn = Id

-- Exercise 6
-- Relative Difficulty: 2
instance Moonad List where
  bind = flatMap
  reeturn = (:| Nil)

-- Exercise 7
-- Relative Difficulty: 2
instance Moonad Optional where
  bind = flip bindOptional
  reeturn = Full

-- Exercise 8
-- Relative Difficulty: 3
instance Moonad ((->) t) where
  -- (a -> ((->) t) b)) -> ((->) t) a -> ((->) t) b
  -- (a -> t -> b) -> (t -> a) -> t -> b
  -- f                  g         x 
  bind f g x = f (g x) x
  reeturn = const

-- Exercise 9
-- Relative Difficulty: 2
instance Moonad IO where
  -- (a -> IO b) -> IO a -> IO b
  bind = error "todo"
  reeturn = error "todo"

-- Exercise 10
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
flaatten = bind id



--  bind :: (a -> m b) -> m a -> m b
--  reeturn :: a -> m a
--  fmaap' :: (a -> b) -> m a -> m b

-- Exercise 11
-- Relative Difficulty: 10
-- bind fmaap'

apply :: Moonad m => m (a -> b) -> m a -> m b
apply mf mx = bind (\x -> fmaap' x mx) mf
--apply mf mx = bind (`fmaap'` mx) mf

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
lift2 mf ma mb = undefined

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 = error "todo"

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 = error "todo"

-- Exercise 15
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence = error "todo"

-- Exercise 16
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse = error "todo"

-- Exercise 17
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate = error "todo"

-- Exercise 18
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
