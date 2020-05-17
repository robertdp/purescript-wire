module Wire.Observable where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event, Subscribe)
import Wire.Event as Event
import Wire.Event.Class (class EventSource)

newtype Observable a b
  = Observable (Event (Either a b))

create :: forall a b. Effect { done :: b -> Effect Unit, observable :: Observable a b, next :: a -> Effect Unit }
create = do
  inner <- Event.create
  result <- Ref.new Nothing
  let
    next a =
      whenM (isNothing <$> Ref.read result) do
        inner.push (Left a)

    done b = do
      Ref.write (Just b) result
      inner.push (Right b)

    event =
      Event.makeEvent \emit -> do
        Ref.read result
          >>= case _ of
              Just b -> emit (Right b) *> mempty
              Nothing -> Event.subscribe inner.event emit
  pure { observable: Observable event, next, done }

subscribe :: forall b a. Observable a b -> Subscribe (Either a b)
subscribe (Observable event) = Event.subscribe event

instance eventSourceObservable :: EventSource (Observable a b) (Either a b) where
  source (Observable event) = event
