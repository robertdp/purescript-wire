module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Wire.Signal as Signal
import Wire.Store.Atom.Types (AtomicF, AtomSignal, Action(..), interpret)

newtype Sync a
  = Sync'
  { default :: a
  , handler :: Action a -> Handler a
  }

type Handler a
  = FreeT (AtomicF a) Effect Unit

createSignal :: forall a. Sync a -> Effect (AtomSignal a)
createSignal (Sync' { default, handler }) = do
  signal <- Signal.create default
  run (handler Initialize) signal
  pure signal

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run = flip interpret

reset :: forall a. Sync a -> AtomSignal a -> Effect Unit
reset (Sync' { default, handler }) signal = do
  signal.write default
  run (handler Initialize) signal

update :: forall a. a -> Sync a -> AtomSignal a -> Effect Unit
update value (Sync' { handler }) = run (handler (Update value))
