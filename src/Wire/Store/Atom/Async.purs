module Wire.Store.Atom.Async where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Wire.Store.Atom.Types (AtomF, AtomStore, Lifecycle(..), createEmptyStore, interpret)

newtype Async a
  = Async' (Lifecycle a -> Handler a)

type Handler a
  = FreeT (AtomF a) Aff Unit

createStore :: forall a. Async a -> Effect (AtomStore a)
createStore (Async' handler) = do
  store <- createEmptyStore
  run (handler Init) store
  pure store

run :: forall a. Handler a -> AtomStore a -> Effect Unit
run handler store = launchAff_ $ interpret store handler

reset :: forall a. Async a -> AtomStore a -> Effect Unit
reset (Async' handler) store@{ write } = do
  write Nothing
  run (handler Init) store

update :: forall a. a -> Async a -> AtomStore a -> Effect Unit
update value (Async' handler) = run (handler (Update value))
