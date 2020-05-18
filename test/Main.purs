module Test.Main where

import Prelude
import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.List.Lazy (range)
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Math as Math
import Wire.Event (Event)
import Wire.Event as Event
import Wire.Event.Time as Time

main :: Effect Unit
main =
  launchAff_ do
    Event.subscribe sqrtOfSumOfSquaresFromOneToOneMillion do Console.log <<< show

seconds :: Event Int
seconds = Event.fold (\n _ -> n + 1) 0 do Time.timer 0 1000

sqrtOfSumOfSquaresFromOneToOneMillion :: Event Number
sqrtOfSumOfSquaresFromOneToOneMillion =
  range 1 1_000_000
    # Event.fromFoldable
    # map Int.toNumber
    # map (\x -> x * x)
    # Event.fold (+) 0.0
    # map Math.sqrt

formatNumber :: String -> String
formatNumber =
  CodeUnits.dropRight 2
    >>> CodeUnits.toCharArray
    >>> Array.reverse
    >>> foldlWithIndex (\i o c -> if i /= 0 && i `mod` 3 == 0 then o <> [ ',', c ] else o <> [ c ]) []
    >>> Array.reverse
    >>> CodeUnits.fromCharArray
