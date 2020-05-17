module Wire.Event.Transformer where

import Prelude
import Control.Alt (alt)
import Data.Compactable (class Compactable, compact, separate)
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filter, filterMap, partition, partitionMap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Wire.Event (Event)

newtype Transformer i o
  = Transformer (Event i -> Event o)

transform :: forall o i. Transformer i o -> Event i -> Event o
transform (Transformer t) = t

transformFlipped :: forall i o. Event i -> Transformer i o -> Event o
transformFlipped = flip transform

infixr 2 transform as <~:

infixl 2 transformFlipped as :~>

lift :: forall i o. (Event i -> Event o) -> Transformer i o
lift = Transformer

derive instance functorTransformer :: Functor (Transformer i)

instance semigroupoidTransformer :: Semigroupoid Transformer where
  compose (Transformer f) (Transformer g) = Transformer (compose f g)

instance categoryTransformer :: Category Transformer where
  identity = Transformer identity

instance profunctorTransformer :: Profunctor Transformer where
  dimap a2b c2d (Transformer t) = Transformer (map c2d <<< t <<< map a2b)

instance choiceTransformer :: Choice Transformer where
  left (Transformer t) = Transformer (separate >>> \event -> alt (map Left (t event.left)) (map Right event.right))
  right (Transformer t) = Transformer (separate >>> \event -> alt (map Left event.left) (map Right (t event.right)))

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
