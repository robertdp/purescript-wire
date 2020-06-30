module Wire.Store.Atom.Pure where

import Prelude
import Wire.Signal as Signal
import Wire.Store.Atom.Class (class Atom)

newtype Pure (key :: Symbol) value
  = Pure value

new :: forall value key. value -> Pure key value
new = Pure

instance atomPure :: Atom (Pure key value) key value where
  create (Pure value) = do
    signal <- Signal.create value
    pure signal
  reset (Pure value) signal = signal.write value
  update _ value signal = signal.write value
