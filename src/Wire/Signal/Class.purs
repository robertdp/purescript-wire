module Wire.Signal.Class where

import Prelude

class
  Monad m <= Readable signal m | signal -> m where
  read :: forall i o. signal i o -> m o
  subscribe :: forall i o. signal i o -> (o -> m Unit) -> m (m Unit)

class
  Monad m <= Writable signal m | signal -> m where
  write :: forall i o. signal i o -> i -> m Unit

subscribe' :: forall signal m i o. Readable signal m => signal i o -> (o -> m Unit) -> m (m Unit)
subscribe' s k = do
  _ <- read s >>= k
  subscribe s k
