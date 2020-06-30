module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Wire.Store.Atom.Types (AtomF, StoreSignal, Action(..), createEmptySignal, interpret)

newtype Sync a
  = Sync' (Action a -> Handler a)

type Handler a
  = FreeT (AtomF a) Effect Unit

createSignal :: forall a. Sync a -> Effect (StoreSignal a)
createSignal (Sync' handler) = do
  signal <- createEmptySignal
  run (handler Initialize) signal
  pure signal

run :: forall a. Handler a -> StoreSignal a -> Effect Unit
run = flip interpret

reset :: forall a. Sync a -> StoreSignal a -> Effect Unit
reset (Sync' handler) signal = run (handler Initialize) signal

update :: forall a. a -> Sync a -> StoreSignal a -> Effect Unit
update value (Sync' handler) = run (handler (Update value))
