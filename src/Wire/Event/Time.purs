module Wire.Event.Time where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Data.Time.Duration (class Duration, fromDuration)
import Effect.AVar as AVar
import Effect.Aff as Aff
import Effect.Aff.AVar as AffVar
import Effect.Class (liftEffect)
import Wire.Event (Event)
import Wire.Event as Event

delay :: forall offset a. Duration offset => offset -> Event a -> Event a
delay offset event = do
  let
    ms = fromDuration offset
  Event.makeEvent \emit -> do
    queue <- AVar.empty
    consumer <-
      (Aff.launchAff <<< forever) do
        a <- AffVar.take queue
        liftEffect do emit a
    cancel <-
      Event.subscribe event \a ->
        Aff.launchAff_ do
          Aff.delay ms
          AffVar.put a queue
    pure do
      cancel
      Aff.launchAff_ do Aff.killFiber (Aff.error "cancelled") consumer
      AVar.kill (Aff.error "cancelled") queue

interval :: forall spacing. Duration spacing => spacing -> Event Unit
interval spacing = do
  let
    ms = fromDuration spacing
  Event.makeEvent \emit -> do
    fiber <-
      (Aff.launchAff <<< forever) do
        Aff.delay ms
        liftEffect do emit unit
    pure do
      Aff.launchAff_ do Aff.killFiber (Aff.error "cancelled") fiber

timer :: forall offset spacing. Duration offset => Duration spacing => offset -> spacing -> Event Unit
timer offset spacing = delay offset do pure unit <|> interval spacing
