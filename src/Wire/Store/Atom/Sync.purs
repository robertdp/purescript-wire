module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Wire.Signal as Signal
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Types (AtomicF, AtomSignal, Action(..), interpret)

newtype Sync (key :: Symbol) value
  = Sync
  { default :: value
  , handler :: Action value -> Handler value
  }

type Handler a
  = FreeT (AtomicF a) Effect Unit

new ::
  forall value key.
  { default :: value
  , handler :: Action value -> FreeT (AtomicF value) Effect Unit
  } ->
  Sync key value
new = Sync

instance atomSync :: Atom (Sync key value) key value where
  create (Sync { default, handler }) = do
    signal <- Signal.create default
    run (handler Initialize) signal
    pure signal
  reset (Sync { default, handler }) signal = do
    signal.write default
    run (handler Initialize) signal
  update (Sync { handler }) value = run (handler (Update value))

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run = flip interpret
