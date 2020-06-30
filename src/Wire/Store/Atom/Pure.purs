module Wire.Store.Atom.Pure where

import Prelude
import Effect (Effect)
import Wire.Store.Atom.Types (AtomStore, createEmptyStore)

newtype Pure a
  = Pure' a

createStore :: forall a. Pure a -> Effect (AtomStore a)
createStore (Pure' value) = do
  store <- createEmptyStore
  store.write (pure value)
  pure store

reset :: forall a. Pure a -> AtomStore a -> Effect Unit
reset (Pure' value) store = store.write (pure value)

update :: forall a. a -> Pure a -> AtomStore a -> Effect Unit
update value _ store = store.write (pure value)
