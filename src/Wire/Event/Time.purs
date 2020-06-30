module Wire.Event.Time where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Data.Time.Duration (class Duration, fromDuration)
import Effect.Aff as Aff
import Wire.Event (Event)
import Wire.Event as Event

delay :: forall offset a. Duration offset => offset -> Event a -> Event a
delay offset event = do
  let
    ms = fromDuration offset
  Event.makeEvent \emit -> do
    Event.subscribe event \next -> do
      Aff.delay ms
      _ <- Aff.forkAff $ emit next
      pure unit

interval :: forall spacing. Duration spacing => spacing -> Event Unit
interval spacing = do
  let
    ms = fromDuration spacing
  Event.makeEvent \emit -> do
    fiber <-
      (Aff.launchAff <<< Aff.supervise <<< forever) do
        Aff.delay ms
        _ <- Aff.forkAff $ emit unit
        pure unit
    pure do
      Aff.launchAff_ do Aff.killFiber (Aff.error "canceling") fiber

timer :: forall offset spacing. Duration offset => Duration spacing => offset -> spacing -> Event Unit
timer offset spacing = delay offset do pure unit <|> interval spacing
