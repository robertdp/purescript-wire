module Wire.Store.Hooks where

import Prelude
import Data.Newtype (class Newtype)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, UseEffect, UseState)
import React.Basic.Hooks as React
import Wire.React.Class (class Atom)
import Wire.React.Class as Class
import Wire.Signal (Signal)
import Wire.Signal as Signal

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
  = UseAtom (UseEffect Unit (UseState a hooks))

derive instance newtypeUseAtom :: Newtype (UseAtom a hooks) _

useAtom :: forall atom a. Atom atom => atom a -> Hook (UseAtom a) (a /\ ((a -> a) -> Effect Unit))
useAtom atom =
  React.coerceHook React.do
    value /\ setValue <- React.useState' $ unsafePerformEffect $ Class.read atom
    React.useEffectOnce $ Signal.subscribe (Class.signal atom) setValue
    pure $ value /\ Class.modify atom

useAtomValue :: forall a atom. Atom atom => atom a -> Hook (UseAtom a) a
useAtomValue atom = fst <$> useAtom atom

newtype UseAtomReset a hooks
  = UseAtomReset (UseAtom a (UseEffect Unit hooks))

derive instance newtypeUseAtomReset :: Newtype (UseAtomReset a hooks) _

useAtomReset :: forall atom a. Atom atom => atom a -> Hook (UseAtomReset a) (a /\ ((a -> a) -> Effect Unit))
useAtomReset atom =
  React.coerceHook React.do
    React.useEffectOnce do
      Class.reset atom
      mempty
    useAtom atom

useAtomResetValue :: forall atom a. Atom atom => atom a -> Hook (UseAtomReset a) a
useAtomResetValue atom = fst <$> useAtomReset atom
