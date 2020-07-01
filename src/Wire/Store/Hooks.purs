module Wire.Store.Hooks where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, UseContext, UseEffect, UseLazy, UseState)
import React.Basic.Hooks as React
import Wire.Signal (Signal)
import Wire.Signal as Signal
import Wire.Store (Store)
import Wire.Store as Store
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Class as Atom
import Wire.Store.Selector (Selector)
import Wire.Store.Selector as Selector

newtype UseSignal a hooks
  = UseSignal (UseEffect Unit (UseState a hooks))

derive instance newtypeUseSignal :: Newtype (UseSignal a hooks) _

useSignal :: forall a. Signal a -> Hook (UseSignal a) a
useSignal signal =
  React.coerceHook React.do
    value /\ setValue <- React.useState' $ unsafePerformEffect $ Signal.read signal
    React.useEffectOnce $ Signal.subscribe signal setValue
    pure value

newtype UseAtom a hooks
  = UseAtom
  ( UseEffect Unit
      ( UseState a
          ( UseLazy Unit { signal :: Signal a, write :: a -> Effect Unit }
              (UseContext Store hooks)
          )
      )
  )

derive instance newtypeUseAtom :: Newtype (UseAtom a hooks) _

useAtom :: forall atom a. Atom atom => atom a -> Hook (UseAtom a) (a /\ (a -> Effect Unit))
useAtom atom =
  React.coerceHook React.do
    store <- React.useContext Store.context
    { signal, write } <- lookupAtom atom store
    value /\ setValue <- React.useState' $ unsafePerformEffect $ Signal.read signal
    React.useEffectOnce $ Signal.subscribe signal setValue
    pure $ value /\ write

useAtomValue :: forall a atom. Atom atom => atom a -> Hook (UseAtom a) a
useAtomValue atom = fst <$> useAtom atom

useResetAtom :: forall atom a. Atom atom => atom a -> Hook (UseAtom a) (a /\ (a -> Effect Unit))
useResetAtom atom =
  React.coerceHook React.do
    store <- React.useContext Store.context
    { signal, write } <- lookupAtom atom store
    value /\ setValue <- React.useState' $ Atom.initialValue atom
    React.useEffectOnce do
      Store.reset atom store
      Signal.subscribe signal setValue
    pure $ value /\ write

useResetAtomValue :: forall a atom. Atom atom => atom a -> Hook (UseAtom a) a
useResetAtomValue atom = fst <$> useAtom atom

lookupAtom ::
  forall a atom.
  Atom atom =>
  atom a ->
  Store ->
  Hook
    (UseLazy Unit { signal :: Signal a, write :: a -> Effect Unit })
    { signal :: Signal a, write :: a -> Effect Unit }
lookupAtom atom store =
  React.useLazy unit \_ -> case Store.unsafeLookup atom store of
    Just storedSignal -> storedSignal
    Nothing ->
      { signal: pure $ Atom.initialValue atom
      , write: mempty
      }

newtype UseSelector a hooks
  = UseSelector
  ( UseEffect Unit
      ( UseState a
          ( UseLazy Unit { signal :: Signal a, write :: a -> Effect Unit }
              (UseContext Store hooks)
          )
      )
  )

derive instance newtypeUseSelector :: Newtype (UseSelector a hooks) _

useSelector :: forall a. Selector a -> Hook (UseSelector a) (a /\ (a -> Effect Unit))
useSelector selector =
  React.coerceHook React.do
    store <- React.useContext Store.context
    { signal, write } <- React.useLazy unit \_ -> Selector.build selector store
    value /\ setValue <- React.useState' $ unsafePerformEffect $ Signal.read signal
    React.useEffectOnce $ Signal.subscribe signal setValue
    pure $ value /\ write

useSelectorValue :: forall a. Selector a -> Hook (UseSelector a) a
useSelectorValue selector = fst <$> useSelector selector
