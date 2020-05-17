module Wire.Event.Time where

import Prelude
import Control.Alt ((<|>))
import Effect.Ref as Ref
import Effect.Timer as Timer
import Wire.Event (Event(..))
import Wire.Event as Event

delay :: forall a. Int -> Event a -> Event a
delay ms event =
  Event.makeEvent \emit -> do
    canceled <- Ref.new false
    cancel <-
      Event.subscribe event \a -> do
        _ <- Timer.setTimeout ms do unlessM (Ref.read canceled) do emit a
        pure unit
    pure do
      Ref.write true canceled
      cancel

interval :: Int -> Event Unit
interval ms =
  Event \emit -> do
    intervalId <- Timer.setInterval ms do emit unit
    pure do Timer.clearInterval intervalId

timer :: Int -> Int -> Event Unit
timer after ms = delay after do pure unit <|> interval ms
