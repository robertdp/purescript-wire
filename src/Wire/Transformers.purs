module Wire.Transformers
  ( module Exports
  , fold
  , distinct
  ) where

import Prelude
import Wire.Event as Event
import Wire.Event.Class (class EventSource, source)
import Wire.Event.Transformer (Transformer(..), lift, transform, transformFlipped, (:~>), (<~:)) as Exports
import Wire.Event.Transformer (Transformer, lift)

fold :: forall a b. (b -> a -> b) -> b -> Transformer a b
fold f b = lift (Event.fold f b)

distinct :: forall a. Eq a => Transformer a a
distinct = lift Event.distinct

buffer :: forall b a source. EventSource source b => source -> Transformer a (Array a)
buffer flush = lift (Event.buffer (source flush))

delay :: forall a. Int -> Transformer a a
delay ms = lift (Event.delay ms)
