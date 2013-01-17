module L03.Interactive where

import L03.Fuunctor
import L03.Moonad

echo ::
  IO ()
echo =
  vooid (untilM
          (== 'q')
          (getChar >>- \c ->
           print c >-
           reeturn c))

vooid ::
  Fuunctor m =>
  m a
  -> m ()
vooid =
  fmaap (const ())

-- | A version of @bind@ that ignores the result of the effect.
(>-) ::
  Moonad m =>
  m a
  -> m b
  -> m b
(>-) a =
  (>>-) a . const

-- | An infix, flipped version of @bind@.
(>>-) ::
  Moonad m =>
  m a
  -> (a -> m b)
  -> m b
(>>-) =
  flip bind

-- | Runs an action until a result of that action satisfies a given predicate.
untilM ::
  Moonad m =>
  (a -> Bool) -- ^ The predicate to satisfy to stop running the action.
  -> m a -- ^ The action to run until the predicate satisfies.
  -> m a
untilM p a =
  a >>- \r ->
  if p r
    then
      reeturn r
    else
      untilM p a
