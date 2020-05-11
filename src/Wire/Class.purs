module Wire.Class where

import Prelude

class
  Monad m <= Readable signal m | signal -> m where
  read :: forall o. signal o -> m o
  subscribe :: forall o. signal o -> (o -> m Unit) -> m (m Unit)

class
  Monad m <= Writable (signal :: Type -> Type -> Type) m where
  write :: forall i o. signal i o -> i -> m Unit
