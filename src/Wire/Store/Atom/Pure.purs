module Wire.Store.Atom.Pure where

import Prelude
import Effect (Effect)
import Wire.Store.Atom.Types (AtomSignal, createEmptySignal)

newtype Pure a
  = Pure' a

createSignal :: forall a. Pure a -> Effect (AtomSignal a)
createSignal (Pure' value) = do
  signal <- createEmptySignal
  signal.write (pure value)
  pure signal

reset :: forall a. Pure a -> AtomSignal a -> Effect Unit
reset (Pure' value) signal = signal.write (pure value)

update :: forall a. a -> Pure a -> AtomSignal a -> Effect Unit
update value _ signal = signal.write (pure value)
