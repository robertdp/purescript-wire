module Test.Main where

import Prelude
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
    Event.subscribe overflow Console.logShow

seconds :: Event Int
seconds = Event.fold (\n _ -> n + 1) 0 do Time.timer 0 1000

overflow :: Event Int
overflow = Event.fromFoldable (range 1 1_000_000)
