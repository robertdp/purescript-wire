module Wire.Signal where

import Prelude
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

type Signal m a
  = { event :: Event a
    , modify :: (a -> a) -> Aff Unit
    , read :: m a
    , write :: a -> Aff Unit
    }

create :: forall m a. MonadEffect m => a -> m (Signal m a)
create init =
  liftEffect do
    value <- Ref.new init
    inner <- Event.create
    let
      read = liftEffect do Ref.read value

      write a = modify (const a)

      modify f = (liftEffect do Ref.modify f value) >>= inner.push

      event =
        Event.makeEvent \emit -> do
          (liftEffect do Ref.read value) >>= emit
          Event.subscribe inner.event emit
    pure { event, read, write, modify }
