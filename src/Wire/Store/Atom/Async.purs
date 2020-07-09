module Wire.Store.Atom.Async where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Wire.Event as Event
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.Store.Atom.Class (class Atom)

newtype Async a
  = Async
  { initial :: a
  , load :: Effect Unit
  , save :: a -> Effect Unit
  , signal :: Signal a
  }

create ::
  forall a.
  { initial :: a
  , load :: Aff a
  , save :: a -> Aff Unit
  } ->
  Effect (Async a)
create { initial, load, save } = do
  signal <- Signal.create initial
  loadFiber <- Ref.new Nothing
  saveFiber <- Ref.new Nothing
  let
    load' = do
      inProgress <- isJust <$> Ref.read loadFiber
      unless inProgress do
        fiber <-
          Aff.launchAff do
            value <- load
            liftEffect $ signal.modify (const value)
        Ref.write (Just fiber) loadFiber

    save' value = do
      inProgress <- isJust <$> Ref.read saveFiber
      when inProgress do
        oldFiber <- Ref.read saveFiber
        for_ oldFiber (Aff.launchAff_ <<< Aff.killFiber (Aff.error "New save triggered, cancelling save in progress"))
      fiber <- Aff.launchAff $ save value
      Ref.write (Just fiber) loadFiber
  load'
  pure $ Async { initial, load: load', save: save', signal }

unsafeCreate ::
  forall a.
  { initial :: a
  , load :: Aff a
  , save :: a -> Aff Unit
  } ->
  Async a
unsafeCreate = unsafePerformEffect <<< create

instance atomAsync :: Atom Async where
  default (Async atom) = atom.initial
  read (Async atom) = atom.signal.read
  modify f (Async atom) = do
    atom.signal.modify f
    atom.signal.read >>= atom.save
  reset (Async atom) = do
    atom.signal.modify (const atom.initial)
    atom.load
  subscribe notify (Async atom) = Event.subscribe atom.signal.event notify
  signal (Async atom) = atom.signal
