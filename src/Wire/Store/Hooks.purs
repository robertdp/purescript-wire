module Wire.Store.Hooks where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, UseEffect, UseState)
import React.Basic.Hooks as React
import Wire.Event as Event
import Wire.Signal (Signal)
import Wire.Store.Atom.Class (class Atom)
import Wire.Store.Atom.Class as Atom

newtype UseSignal a hooks
  = UseSignal (UseEffect Unit (UseState a hooks))

derive instance newtypeUseSignal :: Newtype (UseSignal a hooks) _

useSignal :: forall a. Signal a -> Hook (UseSignal a) a
useSignal signal =
  React.coerceHook React.do
    value /\ setValue <- React.useState' $ unsafePerformEffect signal.read
    React.useEffectOnce $ Event.subscribe signal.event setValue
    pure value

newtype UseAtom a hooks
  = UseAtom (UseEffect Unit (UseState a hooks))

derive instance newtypeUseAtom :: Newtype (UseAtom a hooks) _

useAtom :: forall atom a. Atom atom => atom a -> Hook (UseAtom a) (a /\ ((a -> a) -> Effect Unit))
useAtom atom =
  React.coerceHook React.do
    value /\ setValue <- React.useState' $ unsafePerformEffect $ Atom.read atom
    React.useEffectOnce $ Event.subscribe (Atom.signal atom).event setValue
    pure $ value /\ flip Atom.modify atom

useAtomValue :: forall a atom. Atom atom => atom a -> Hook (UseAtom a) a
useAtomValue atom = fst <$> useAtom atom

newtype UseAtomReset a hooks
  = UseAtomReset (UseAtom a (UseEffect Unit hooks))

derive instance newtypeUseAtomReset :: Newtype (UseAtomReset a hooks) _

useAtomReset :: forall atom a. Atom atom => atom a -> Hook (UseAtomReset a) (a /\ ((a -> a) -> Effect Unit))
useAtomReset atom =
  React.coerceHook React.do
    React.useEffectOnce do
      Atom.reset atom
      mempty
    useAtom atom

useAtomResetValue :: forall atom a. Atom atom => atom a -> Hook (UseAtomReset a) a
useAtomResetValue atom = fst <$> useAtomReset atom
