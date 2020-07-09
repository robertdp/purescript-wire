module Wire.React.Class where

import Prelude
import Effect (Effect)
import Wire.Signal (Signal)

class Atom (atom :: Type -> Type) where
  default :: forall value. atom value -> value
  read :: atom ~> Effect
  modify :: forall value. atom value -> (value -> value) -> Effect Unit
  reset :: forall value. atom value -> Effect Unit
  subscribe :: forall value. atom value -> (value -> Effect Unit) -> Effect (Effect Unit)
  signal :: atom ~> Signal
