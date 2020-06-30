module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Wire.Store.Atom.Types (AtomF, AtomSignal, Lifecycle(..), createEmptySignal, interpret)

newtype Sync a
  = Sync' (Lifecycle a -> Handler a)

type Handler a
  = FreeT (AtomF a) Effect Unit

createSignal :: forall a. Sync a -> Effect (AtomSignal a)
createSignal (Sync' handler) = do
  signal <- createEmptySignal
  run (handler Init) signal
  pure signal

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run = flip interpret

reset :: forall a. Sync a -> AtomSignal a -> Effect Unit
reset (Sync' handler) signal = run (handler Init) signal

update :: forall a. a -> Sync a -> AtomSignal a -> Effect Unit
update value (Sync' handler) = run (handler (Update value))
