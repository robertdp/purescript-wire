module Wire.Transformers
  ( module Exports
  , fold
  , distinct
  ) where

import Prelude
import Wire.Event as Event
import Wire.Event.Class (class EventSource, source)
import Wire.Event.Time as Time
import Wire.Event.Transformer (Transformer(..), lift, transform, transformFlipped, (:~>), (<~:)) as Exports
import Wire.Event.Transformer (Transformer, lift)

fold :: forall a b. (b -> a -> b) -> b -> Transformer a b
fold f b = lift (Event.fold f b)

distinct :: forall a. Eq a => Transformer a a
distinct = lift Event.distinct

bufferUntil :: forall b a source. EventSource source b => source -> Transformer a (Array a)
bufferUntil flush = lift (Event.bufferUntil (source flush))

delay :: forall a. Int -> Transformer a a
delay ms = lift (Time.delay ms)
