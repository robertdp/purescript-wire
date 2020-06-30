module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Wire.Store.Atom.Types (AtomF, AtomSignal, Lifecycle(..), createEmptySignal, interpret)

newtype Async a
  = Async' (Lifecycle a -> Handler a)

type Handler a
  = FreeT (AtomF a) Aff Unit

createSignal :: forall a. Async a -> Effect (AtomSignal a)
createSignal (Async' handler) = do
  signal <- createEmptySignal
  run (handler Init) signal
  pure signal

run :: forall a. Handler a -> AtomSignal a -> Effect Unit
run handler signal = launchAff_ $ interpret signal handler

reset :: forall a. Async a -> AtomSignal a -> Effect Unit
reset (Async' handler) signal = do
  signal.write Nothing
  run (handler Init) signal

update :: forall a. a -> Async a -> AtomSignal a -> Effect Unit
update value (Async' handler) = run (handler (Update value))
