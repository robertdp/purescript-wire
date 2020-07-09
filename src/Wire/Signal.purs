module Wire.Signal where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

type Signal a
  = { event :: Event a
    , read :: Effect a
    , modify :: (a -> a) -> Effect Unit
    }

create :: forall a. a -> Effect (Signal a)
create init = do
  value <- Ref.new init
  inner <- Event.create
  let
    read = Ref.read value

    event =
      Event.makeEvent \notify -> do
        unsubscribe <- Event.subscribe inner.event notify
        read >>= notify
        pure unsubscribe

    modify f = Ref.modify f value >>= inner.push
  pure { event, read, modify }
