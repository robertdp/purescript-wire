module Wire.Observable where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Ref as Ref
import Wire.Event (Event)
import Wire.Event as Event

type Observable a b
  = Event (Either a b)

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

    observable =
      Event.makeEvent \emit -> do
        Ref.read result
          >>= case _ of
              Just b -> emit (Right b) *> mempty
              Nothing -> Event.subscribe inner.event emit
  pure { observable, next, done }
