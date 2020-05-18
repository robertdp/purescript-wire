module Wire.Event.Time where

import Prelude
import Control.Alt ((<|>))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Timer as Timer
import Wire.Event (Event)
import Wire.Event as Event

delay :: forall a. Int -> Event a -> Event a
delay ms event =
  Event.makeEvent \emit -> do
    canceled <- liftEffect do Ref.new false
    cancel <-
      Event.subscribe event \a ->
        liftEffect do
          _ <- Timer.setTimeout ms do unlessM (Ref.read canceled) do Aff.launchAff_ do emit a
          pure unit
    pure do
      liftEffect do Ref.write true canceled
      cancel

interval :: Int -> Event Unit
interval ms =
  Event.makeEvent \emit ->
    liftEffect do
      intervalId <- Timer.setInterval ms do Aff.launchAff_ do emit unit
      pure do liftEffect do Timer.clearInterval intervalId

timer :: Int -> Int -> Event Unit
timer after ms = delay after do pure unit <|> interval ms
