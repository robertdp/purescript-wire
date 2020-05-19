module Test.Main where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.List.Lazy (range)
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Class.Console as Console
import Wire.Event (Event)
import Wire.Event as Event

main :: Effect Unit
main = do
  void $ Event.subscribe (Event.distinct (sumFromOneToOneMillion <|> sumFromOneToOneMillion) >>= pure <<< formatNumber <<< show) do Console.log

sumFromOneToOneMillion :: Event Number
sumFromOneToOneMillion =
  range 1 1_000_000
    # Event.fromFoldable
    # map Int.toNumber
    # Event.fold (+) 0.0

formatNumber :: String -> String
formatNumber =
  CodeUnits.dropRight 2
    >>> CodeUnits.toCharArray
    >>> Array.reverse
    >>> foldlWithIndex (\i o c -> if i /= 0 && i `mod` 3 == 0 then o <> [ ',', c ] else o <> [ c ]) []
    >>> Array.reverse
    >>> CodeUnits.fromCharArray
