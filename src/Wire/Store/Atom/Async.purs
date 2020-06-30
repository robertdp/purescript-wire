module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Wire.Store.Atom.Types (AtomF, StoreSignal, Action(..), createEmptySignal, interpret)

newtype Async a
  = Async' (Action a -> Handler a)

type Handler a
  = FreeT (AtomF a) Aff Unit

createSignal :: forall a. Async a -> Effect (StoreSignal a)
createSignal (Async' handler) = do
  signal <- createEmptySignal
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
