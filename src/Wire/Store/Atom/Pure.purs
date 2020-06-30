module Wire.Store.Atom.Pure where

import Prelude
import Effect (Effect)
import Wire.Signal as Signal
import Wire.Store.Atom.Types (AtomSignal)

newtype Pure a
  = Pure' a

createSignal :: forall a. Pure a -> Effect (AtomSignal a)
createSignal (Pure' value) = do
  signal <- Signal.create value
  pure signal

reset :: forall a. Pure a -> AtomSignal a -> Effect Unit
reset (Pure' value) signal = signal.write value

update :: forall a. a -> Pure a -> AtomSignal a -> Effect Unit
update value _ signal = signal.write value
