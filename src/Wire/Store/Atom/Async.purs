module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Wire.Signal as Signal
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Types (AtomicF, AtomSignal, Action(..), interpret)

newtype Async (key :: Symbol) value
  = Async
  { default :: value
  , handler :: Action value -> Handler value
  }

new ::
  forall value key.
  { default :: value
  , handler :: Action value -> FreeT (AtomicF value) Aff Unit
  } ->
  Async key value
new = Async

type Handler a
  = FreeT (AtomicF a) Aff Unit

instance atomAsync :: Atom Async where
  create (Async { default, handler }) = do
    signal <- Signal.create default
    run (handler Initialize) signal
    pure signal
  reset (Async { default, handler }) signal = do
    signal.write default
    run (handler Initialize) signal
  update (Async { handler }) value = run (handler (Update value))

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run handler signal = launchAff_ $ interpret signal handler
