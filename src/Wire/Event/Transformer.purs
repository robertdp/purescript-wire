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
  dimap a2b c2d (Transformer t) = Transformer (map c2d <<< t <<< map a2b)

instance choiceTransformer :: Choice Transformer where
  left (Transformer a2b) =
    Transformer \eventAC ->
      let
        { left: eventA, right: eventC } = separate eventAC
      in
        alt (map Left (a2b eventA)) (map Right eventC)
  right (Transformer b2c) =
    Transformer \eventAB ->
      let
        { left: eventA, right: eventB } = separate eventAB
      in
        alt (map Left eventA) (map Right (b2c eventB))

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
