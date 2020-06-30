module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Wire.Signal as Signal
import Wire.Store.Atom.Types (AtomicF, AtomSignal, Action(..), interpret)

newtype Async a
  = Async'
  { default :: a
  , handler :: Action a -> Handler a
  }

type Handler a
  = FreeT (AtomicF a) Aff Unit

createSignal :: forall a. Async a -> Effect (AtomSignal a)
createSignal (Async' { default, handler }) = do
  signal <- Signal.create default
  run (handler Initialize) signal
  pure signal

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run handler signal = launchAff_ $ interpret signal handler

reset :: forall a. Async a -> AtomSignal a -> Effect Unit
reset (Async' { default, handler }) signal = do
  signal.write default
  run (handler Initialize) signal

update :: forall a. a -> Async a -> AtomSignal a -> Effect Unit
update value (Async' { handler }) = run (handler (Update value))
