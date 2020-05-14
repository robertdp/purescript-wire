module Wire.Event.Transformer where

import Prelude
import Control.Alt (alt)
import Data.Compactable (class Compactable, compact, separate)
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filter, filterMap, partition, partitionMap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Wire.Event (Event, makeEvent, subscribe)
import Wire.Event.Class (class EventSource, source)

newtype Transformer i o
  = Transformer (Event i -> Event o)

connect :: forall i source o. EventSource source i => source -> Transformer i o -> Event o
connect from transformer = transform transformer (source from)

infixl 2 connect as ==>

lift :: forall i o. (Event i -> Event o) -> Transformer i o
lift = Transformer

transform :: forall i o. Transformer i o -> Event i -> Event o
transform (Transformer t) = t

derive instance functorTransformer :: Functor (Transformer i)

instance semigroupoidTransformer :: Semigroupoid Transformer where
  compose (Transformer f) (Transformer g) = Transformer (compose f g)

instance categoryTransformer :: Category Transformer where
  identity = Transformer identity

instance profunctorTransformer :: Profunctor Transformer where
  dimap a2b c2d (Transformer b2c) =
    Transformer \eventA ->
      let
        eventB =
          makeEvent \emitB ->
            subscribe eventA \a -> do
              emitB (a2b a)

        eventC = b2c eventB

        eventD =
          makeEvent \emitD -> do
            subscribe eventC \c ->
              emitD (c2d c)
      in
        eventD

instance choiceTransformer :: Choice Transformer where
  left (Transformer a2b) =
    Transformer \eventAC ->
      let
        { left: eventA, right: eventC } = separate eventAC

        eventB = a2b eventA

        eventBC = alt (map Left eventB) (map Right eventC)
      in
        eventBC
  right (Transformer b2c) =
    Transformer \eventAB ->
      let
        { left: eventA, right: eventB } = separate eventAB

        eventC = b2c eventB

        eventAC = alt (map Left eventA) (map Right eventC)
      in
        eventAC

instance compactableTransformer :: Compactable (Transformer i) where
  compact (Transformer t) = Transformer (compact <<< t)
  separate (Transformer t) =
    { left: Transformer (_.left <<< separate <<< t)
    , right: Transformer (_.right <<< separate <<< t)
    }

instance filterableTransformer :: Filterable (Transformer i) where
  partitionMap f (Transformer t) =
    { left: Transformer (_.left <<< partitionMap f <<< t)
    , right: Transformer (_.right <<< partitionMap f <<< t)
    }
  partition f (Transformer t) =
    { yes: Transformer (_.yes <<< partition f <<< t)
    , no: Transformer (_.no <<< partition f <<< t)
    }
  filterMap f (Transformer t) = Transformer (filterMap f <<< t)
  filter f (Transformer t) = Transformer (filter f <<< t)
