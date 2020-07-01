module Wire.Store.Atom.Class where

import Prelude
import Effect (Effect)
import Wire.Store.Atom (AtomSignal)

class Atom (atom :: Type -> Type) where
  storeKey :: forall value. atom value -> String
  initialValue :: forall value. atom value -> value
  isInitialised :: forall value. atom value -> Effect Boolean
  initialise :: forall value. atom value -> AtomSignal value -> Effect Unit
  reset :: forall value. atom value -> AtomSignal value -> Effect Unit
  update :: forall value. atom value -> value -> AtomSignal value -> Effect Unit
