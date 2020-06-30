module Wire.Store.Atom.Class where

import Prelude
import Effect (Effect)
import Wire.Store.Atom.Types (AtomSignal)

class Atom atom (key :: Symbol) value | atom -> key value where
  create :: atom -> Effect (AtomSignal value)
  reset :: atom -> AtomSignal value -> Effect Unit
  update :: atom -> value -> AtomSignal value -> Effect Unit
