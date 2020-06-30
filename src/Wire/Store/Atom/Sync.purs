module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Wire.Signal as Signal
import Wire.Store.Atom.Types (AtomicF, StoreSignal, Action(..), interpret)

newtype Sync a
  = Sync' (Action a -> Handler a)

type Handler a
  = FreeT (AtomicF a) Effect Unit

createSignal :: forall a. Sync a -> Effect (StoreSignal a)
createSignal (Sync' handler) = do
  signal <- Signal.create Nothing
  run (handler Initialize) signal
  pure signal

run :: forall a. Handler a -> StoreSignal a -> Effect Unit
run = flip interpret

reset :: forall a. Sync a -> StoreSignal a -> Effect Unit
reset (Sync' handler) signal = run (handler Initialize) signal

update :: forall a. a -> Sync a -> StoreSignal a -> Effect Unit
update value (Sync' handler) = run (handler (Update value))
