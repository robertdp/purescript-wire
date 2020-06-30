module Wire.Event.Transformer where

import Prelude
import Control.Alt (class Alt, alt)
import Data.Compactable (class Compactable, compact, separate)
import Data.Either (Either(..))
import Data.Filterable (class Filterable, filter, filterMap, partition, partitionMap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)

newtype Transformer f i o
  = Transformer (f i -> f o)

transform :: forall f o i. Transformer f i o -> f i -> f o
transform (Transformer t) = t

transformFlipped :: forall f i o. f i -> Transformer f i o -> f o
transformFlipped = flip transform

infixr 2 transform as <~:

infixl 2 transformFlipped as :~>

lift :: forall f i o. (f i -> f o) -> Transformer f i o
lift = Transformer

derive instance functorTransformer :: Functor f => Functor (Transformer f i)

instance semigroupoidTransformer :: Semigroupoid (Transformer f) where
  compose (Transformer f) (Transformer g) = Transformer (compose f g)

instance categoryTransformer :: Category (Transformer f) where
  identity = Transformer identity

instance profunctorTransformer :: Functor f => Profunctor (Transformer f) where
  dimap a2b c2d (Transformer t) = Transformer (map c2d <<< t <<< map a2b)

instance choiceTransformer :: (Alt f, Compactable f) => Choice (Transformer f) where
  left (Transformer t) = Transformer (separate >>> \event -> alt (Left <$> t event.left) (Right <$> event.right))
  right (Transformer t) = Transformer (separate >>> \event -> alt (Left <$> event.left) (Right <$> t event.right))

instance compactableTransformer :: Compactable f => Compactable (Transformer f i) where
  compact (Transformer t) = Transformer (compact <<< t)
  separate (Transformer t) =
    { left: Transformer (_.left <<< separate <<< t)
    , right: Transformer (_.right <<< separate <<< t)
    }

instance filterableTransformer :: Filterable f => Filterable (Transformer f i) where
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
