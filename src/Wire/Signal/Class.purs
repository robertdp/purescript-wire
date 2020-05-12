module Wire.Signal.Class where

import Prelude

class
  Monad m <= Readable signal m | signal -> m where
  read :: forall i o. signal i o -> m o
  subscribe :: forall i o. (o -> m Unit) -> signal i o -> m (m Unit)

class
  Monad m <= Writable signal m | signal -> m where
  write :: forall i o. i -> signal i o -> m Unit

immediately :: forall signal m i o. Readable signal m => (o -> m Unit) -> signal i o -> m (m Unit)
immediately k s = do
  _ <- read s >>= k
  subscribe k s

modify :: forall i signal o m. Readable signal m => Writable signal m => (o -> i) -> signal i o -> m Unit
modify f s = read s >>= f >>> flip write s
