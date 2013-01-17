module L03.Interactive where

import L03.Fuunctor
import L03.Moonad
import Data.List(find)

data Op =
  Op
    Char -- keyboard entry
    String -- description
    (IO ()) -- program

-- convert to upper
-- reverse file
-- encode URL
interactive ::
  IO ()
interactive =
  let ops = [
              Op 'c' "Convert a string to upper-case" (print "convert")
            , Op 'r' "Reverse a file" (print "reverse")
            , Op 'e' "Encode a URL" (print "encode")
            ]
  in vooid (untilM
             (\c ->
               if c == 'q'
                 then
                   putStrLn "Bye!" >-
                   reeturn True
                 else
                   reeturn False)
             (putStrLn "Select: " >-
              traaverse (\(Op c s _) ->
                putStr [c] >-
                putStr ". " >-
                putStrLn s) ops >-
              putStrLn "q. Quit" >-
              getChar >>- \c ->
              let o = find (\(Op c' _ _) -> c' == c) ops
                  r = case o of
                        Nothing -> id
                        Just (Op _ _ q) -> (q >-)
              in r (return c)))

echo ::
  IO ()
echo =
  vooid (untilM
          (\c ->
            if c == 'q'
              then
                putStrLn "Bye!" >-
                reeturn True
              else
                reeturn False)
          (putStr "Enter a character: " >-
           getChar >>- \c ->
           putStrLn "" >-
           putStrLn [c] >-
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
  (a -> m Bool) -- ^ The predicate to satisfy to stop running the action.
  -> m a -- ^ The action to run until the predicate satisfies.
  -> m a
untilM p a =
  a >>- \r ->
  p r >>- \q ->
  if q
    then
      reeturn r
    else
      untilM p a
