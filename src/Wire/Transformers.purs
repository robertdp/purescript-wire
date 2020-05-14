module Wire.Transformers where

import Prelude
import Wire.Event as Event
import Wire.Event.Transformer (Transformer, lift)

fold :: forall a b. (b -> a -> b) -> b -> Transformer a b
fold f b = lift (Event.fold f b)

distinct :: forall a. Eq a => Transformer a a
distinct = lift Event.distinct
