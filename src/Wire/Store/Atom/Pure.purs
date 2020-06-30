module Wire.Store.Atom.Pure where

import Prelude
import Effect (Effect)
import Wire.Store.Atom.Types (StoreSignal, createEmptySignal)

newtype Pure a
  = Pure' a

createSignal :: forall a. Pure a -> Effect (StoreSignal a)
createSignal (Pure' value) = do
  signal <- createEmptySignal
  signal.write (pure value)
  pure signal

reset :: forall a. Pure a -> StoreSignal a -> Effect Unit
reset (Pure' value) signal = signal.write (pure value)

update :: forall a. a -> Pure a -> StoreSignal a -> Effect Unit
update value _ signal = signal.write (pure value)
