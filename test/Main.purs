module Test.Main where

import Prelude
import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Class.Console as Console
import Wire.Event (Event)
import Wire.Event as Event

main :: Effect Unit
main = do
  void $ Event.subscribe ((\a b -> "Hi " <> show (a + b)) <$> Event.range 1 5 <*> (Event.range 6 10 >>= pure)) do Console.logShow

sumOfSquaresFromOneToOneThousand :: Event Number
sumOfSquaresFromOneToOneThousand =
  Event.range 1 1_000
    # map Int.toNumber
    # map (\x -> x * x)
    # Event.fold (+) 0.0

formatNumber :: String -> String
formatNumber =
  CodeUnits.dropRight 2
    >>> CodeUnits.toCharArray
    >>> Array.reverse
    >>> foldlWithIndex (\i o c -> if i /= 0 && i `mod` 3 == 0 then o <> [ ',', c ] else o <> [ c ]) []
    >>> Array.reverse
    >>> CodeUnits.fromCharArray
