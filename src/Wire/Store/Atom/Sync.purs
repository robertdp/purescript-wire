module Wire.Store.Atom.Sync where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Effect (Effect)
import Wire.Store.Atom.Types (AtomF, AtomStore, Lifecycle(..), createEmptyStore, interpret)

newtype Sync a
  = Sync' (Lifecycle a -> Handler a)

type Handler a
  = FreeT (AtomF a) Effect Unit

createStore :: forall a. Sync a -> Effect (AtomStore a)
createStore (Sync' handler) = do
  store <- createEmptyStore
  run (handler Init) store
  pure store

run :: forall a. Handler a -> AtomStore a -> Effect Unit
run = flip interpret

reset :: forall a. Sync a -> AtomStore a -> Effect Unit
reset (Sync' handler) store@{ write } = do
  run (handler Init) store

update :: forall a. a -> Sync a -> AtomStore a -> Effect Unit
update value (Sync' handler) = run (handler (Update value))
