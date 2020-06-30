module Test.Main where

import Prelude
import Data.Array as Array
import Data.Filterable (filter)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Wire.Event (Event)
import Wire.Event as Event

main :: Effect Unit
main = do
  void $ Event.subscribe (combineWithProduct <$> sumOfSquaresFromOneToOneThousand <*> sumOfSquaresFromOneToOneThousand) logEquation

combineWithProduct :: Number -> Number -> { left :: Number, right :: Number, product :: Number }
combineWithProduct left right = { left, right, product: left * right }

logEquation :: forall m. MonadEffect m => { left :: Number, product :: Number, right :: Number } -> m Unit
logEquation { left, right, product } = Console.log (formatNumber left <> " * " <> formatNumber right <> " = " <> formatNumber product)

sumOfSquaresFromOneToOneThousand :: Event Number
sumOfSquaresFromOneToOneThousand =
  Event.range 1 1_000
    # map Int.toNumber
    # map (\x -> x * x)
    # Event.fold (+) 0.0

formatNumber :: Number -> String
formatNumber =
  show >>> CodeUnits.dropRight 2
    >>> CodeUnits.toCharArray
    >>> Array.reverse
    >>> foldlWithIndex (\i o c -> if i /= 0 && i `mod` 3 == 0 then o <> [ ',', c ] else o <> [ c ]) []
    >>> Array.reverse
    >>> CodeUnits.fromCharArray
