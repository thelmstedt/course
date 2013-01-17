module L03.Interactive where


untilM ::
  Monad m =>
  (a -> m Bool)
  -> m a
  -> m a
untilM p a =
  do r <- a
     q <- p r
     if q
       then
         return r
      else
         untilM p a
