module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Wire.Signal as Signal
import Wire.Store.Atom.Types (StateF, StoreSignal, Action(..), interpret)

newtype Async a
  = Async'
  { default :: a
  , handler :: Action a -> Handler a
  }

type Handler a
  = FreeT (StateF a) Aff Unit

createSignal :: forall a. Async a -> Effect (StoreSignal a)
createSignal (Async' { default, handler }) = do
  signal <- Signal.create default
  run (handler Initialize) signal
  pure signal

run :: forall a. Handler a -> StoreSignal a -> Effect Unit
run handler signal = launchAff_ $ interpret signal handler

reset :: forall a. Async a -> StoreSignal a -> Effect Unit
reset (Async' { default, handler }) signal = do
  signal.write default
  run (handler Initialize) signal

update :: forall a. a -> Async a -> StoreSignal a -> Effect Unit
update value (Async' { handler }) = run (handler (Update value))
