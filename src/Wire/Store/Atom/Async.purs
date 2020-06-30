module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Wire.Signal as Signal
import Wire.Store.Atom.Types (AtomicF, StoreSignal, Action(..), interpret)

newtype Async a
  = Async' (Action a -> Handler a)

type Handler a
  = FreeT (AtomicF a) Aff Unit

createSignal :: forall a. Async a -> Effect (StoreSignal a)
createSignal (Async' handler) = do
  signal <- Signal.create Nothing
  run (handler Initialize) signal
  pure signal

run :: forall a. Handler a -> StoreSignal a -> Effect Unit
run handler signal = launchAff_ $ interpret signal handler

reset :: forall a. Async a -> StoreSignal a -> Effect Unit
reset (Async' handler) signal = do
  signal.write Nothing
  run (handler Initialize) signal

update :: forall a. a -> Async a -> StoreSignal a -> Effect Unit
update value (Async' handler) = run (handler (Update value))
