module Wire.Transformer
  ( module Exports
  , fold
  , distinct
  ) where

import Prelude
import Wire.Event (Event)
import Wire.Event as Event
import Wire.Event.Time as Time
import Wire.Event.Transformer (Transformer(..), lift, transform, transformFlipped, (:~>), (<~:)) as Exports
import Wire.Event.Transformer (Transformer, lift)

fold :: forall a b. (b -> a -> b) -> b -> Transformer a b
fold f b = lift (Event.fold f b)

distinct :: forall a. Eq a => Transformer a a
distinct = lift Event.distinct

bufferUntil :: forall b a. Event b -> Transformer a (Array a)
bufferUntil flush = lift (Event.bufferUntil flush)

delay :: forall a. Int -> Transformer a a
delay ms = lift (Time.delay ms)
