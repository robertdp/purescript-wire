module Wire.React.Async where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Wire.React.Class (class Atom)
import Wire.Signal (Signal)
import Wire.Signal as Signal

newtype Async a
  = Async
  { initial :: a
  , load :: Effect Unit
  , save :: a -> Effect Unit
  , signal :: Signal a
  , modify :: (a -> a) -> Effect Unit
  }

create ::
  forall a.
  { initial :: a
  , load :: Aff a
  , save :: a -> Aff Unit
  } ->
  Effect (Async a)
create { initial, load, save } = do
  { signal, modify } <- Signal.create initial
  loadFiber <- Ref.new Nothing
  saveFiber <- Ref.new Nothing
  let
    load' = do
      inProgress <- isJust <$> Ref.read loadFiber
      unless inProgress do
        fiber <-
          Aff.launchAff do
            value <- load
            liftEffect $ modify (const value)
        Ref.write (Just fiber) loadFiber

    save' value = do
      inProgress <- isJust <$> Ref.read saveFiber
      when inProgress do
        oldFiber <- Ref.read saveFiber
        for_ oldFiber (Aff.launchAff_ <<< Aff.killFiber (Aff.error "New save triggered, cancelling save in progress"))
      fiber <- Aff.launchAff $ save value
      Ref.write (Just fiber) loadFiber
  load'
  pure $ Async { initial, load: load', save: save', signal, modify }

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
  read (Async atom) = Signal.read atom.signal
  modify (Async atom) f = do
    atom.modify f
    Signal.read atom.signal >>= atom.save
  reset (Async atom) = do
    atom.modify (const atom.initial)
    atom.load
  subscribe (Async atom) = Signal.subscribe atom.signal
  signal (Async atom) = atom.signal
