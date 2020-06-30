module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Wire.Signal as Signal
import Wire.Store.Atom.Types (StateF, StoreSignal, Action(..), interpret)

newtype Sync a
  = Sync'
  { default :: a
  , handler :: Action a -> Handler a
  }

type Handler a
  = FreeT (StateF a) Effect Unit

createSignal :: forall a. Sync a -> Effect (StoreSignal a)
createSignal (Sync' { default, handler }) = do
  signal <- Signal.create default
  run (handler Initialize) signal
  pure signal

run :: forall a. Handler a -> StoreSignal a -> Effect Unit
run = flip interpret

reset :: forall a. Sync a -> StoreSignal a -> Effect Unit
reset (Sync' { default, handler }) signal = do
  signal.write default
  run (handler Initialize) signal

update :: forall a. a -> Sync a -> StoreSignal a -> Effect Unit
update value (Sync' { handler }) = run (handler (Update value))
