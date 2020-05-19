module Wire.Event.Time where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds)
import Effect.Aff as Aff
import Wire.Event (Event)
import Wire.Event as Event

delay :: forall a. Milliseconds -> Event a -> Event a
delay ms event =
  Event.makeEvent \emit -> do
    cancel <-
      Event.subscribe event \a -> do
        Aff.delay ms
        emit a
    pure cancel

interval :: Milliseconds -> Event Unit
interval ms =
  Event.makeEvent \emit -> do
    fiber <-
      Aff.forkAff do
        forever do
          Aff.delay ms
          emit unit
    pure do
      Aff.killFiber (Aff.error "cancelling") fiber

timer :: Milliseconds -> Milliseconds -> Event Unit
timer after ms = delay after do pure unit <|> interval ms
