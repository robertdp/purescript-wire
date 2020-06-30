module Wire.Store.Atom where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Effect.Aff (Aff)
import Wire.Store.Atom.Async (Async)
import Wire.Store.Atom.Async as Async
import Wire.Store.Atom.Pure (Pure)
import Wire.Store.Atom.Pure as Pure
import Wire.Store.Atom.Sync (Sync)
import Wire.Store.Atom.Sync as Sync
import Wire.Store.Atom.Types (Action, AtomicF)

newAsync ::
  forall value key.
  { default :: value
  , handler :: Action value -> FreeT (AtomicF value) Aff Unit
  } ->
  Async key value
newAsync = Async.new

newSync ::
  forall value key.
  { default :: value
  , handler :: Action value -> FreeT (AtomicF value) Effect Unit
  } ->
  Sync key value
newSync = Sync.new

newPure :: forall value key. value -> Pure key value
newPure = Pure.new
