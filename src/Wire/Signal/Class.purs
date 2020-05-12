module Wire.Signal.Class where

import Prelude

class
  Monad m <= Readable signal m | signal -> m where
  read :: forall i o. signal i o -> m o
  subscribe :: forall i o. signal i o -> (o -> m Unit) -> m (m Unit)

class
  Monad m <= Writable signal m | signal -> m where
  write :: forall i o. signal i o -> i -> m Unit

immediately :: forall signal m i o. Readable signal m => signal i o -> (o -> m Unit) -> m (m Unit)
immediately s k = do
  _ <- read s >>= k
  subscribe s k

modify :: forall i signal o m. Readable signal m => Writable signal m => signal i o -> (o -> i) -> m Unit
modify s f = read s >>= f >>> write s
