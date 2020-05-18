module Test.Main where

import Prelude
import Data.Int as Int
import Data.List.Lazy (range)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Wire.Event (Event)
import Wire.Event as Event
import Wire.Event.Time as Time

main :: Effect Unit
main =
  launchAff_ do
    Event.subscribe sumFromOneToOneMillion Console.logShow

seconds :: Event Int
seconds = Event.fold (\n _ -> n + 1) 0 do Time.timer 0 1000

sumFromOneToOneMillion :: Event Number
sumFromOneToOneMillion =
  range 1 1_000_000
    # Event.fromFoldable
    # map Int.toNumber
    # Event.fold (+) 0.0
