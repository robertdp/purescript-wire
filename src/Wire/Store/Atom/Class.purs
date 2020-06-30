module Wire.Store.Atom.Class where

import Prelude
import Effect (Effect)
import Wire.Store.Atom.Types (AtomSignal)

class Atom (atom :: Symbol -> Type -> Type) where
  create :: forall key value. atom key value -> Effect (AtomSignal value)
  reset :: forall key value. atom key value -> AtomSignal value -> Effect Unit
  update :: forall key value. atom key value -> value -> AtomSignal value -> Effect Unit
