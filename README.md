# purescript-wire

Wire aims to provide useful reactive tools: currently `Event` and `Signal`.

## Wire.Event

`Event` models asynchronous, discreet events over time, and are roughly analogous to Observables in RxJS.

Note: this implementation was largely inspired by `FRP.Event` from the [purescript-event](https://github.com/paf31/purescript-event) library. My implementation has a more restricted set of functionality, has immediate unsubscription (which works better when combined with React), and a monad instance.

### Creating an event

```purescript
create :: forall a. Effect { event :: Event a, push :: a -> Effect Unit }
```

Example:

```purescript
periodically :: Milliseconds -> Effect (Event Unit)
periodically (Milliseconds ms) = do
  { event, push } <- Event.create
  setInterval (Math.floor ms) do
    push unit
```

### Subscribing to an event

```purescript
subscribe :: forall a b. Event a -> (a -> Effect b) -> Effect (Effect Unit)
```

Example:

```purescript
logToConsole :: forall a. Show a => Event a -> Effect (Effect Unit)
logToConsole event = Event.subscribe event Console.logShow
```

The `Effect Unit` being returned from `Event.subscribe` is the subscription canceller.

### Extra functionality

- `fold :: forall a b. (b -> a -> b) -> b -> Event a -> Event b`
- `share :: forall a. Event a -> Effect (Event a)` (see [#12](https://github.com/robertdp/purescript-wire/issues/12))
- `distinct :: forall a. Eq a => Event a -> Event a`
- `bufferUntil :: forall b a. Event a -> Event b -> Event (Array a)`
- `fromFoldable :: forall a f. Foldable f => f a -> Event a`
- `range :: Int -> Int -> Event Int`
- `times :: Int -> Event Int`

## Wire.Signal

`Signal` is like `Event` but it models a continuous value over time, with change events. As such you can always read the current value of a signal, as well as subscribe to future changes. Signals are build on top of `Event`, and also have a monad instance.

This was originally created for modelling application-level reactive state in React, for things like routing and user authentication.

### Creating a signal

```purescript
create :: forall a. a -> Effect { signal :: Signal a, modify :: (a -> a) -> Effect Unit }
```

Example:

```purescript
createAuthSignal :: Effect { auth :: Signal (Maybe AuthData), login :: AuthData -> Effect Unit, logout :: Effect Unit }
createAuthSignal = do
  { signal, modify } <- Signal.create Nothing
  pure
    { auth: signal
    , login: \authData -> modify (const (Just authData))
    , logout: modify (const Nothing)
    }
```

### Sampling a signal

```purescript
read :: forall a. Signal a -> Effect a
```

### Subscribing to a signal

Subscribing to a signal is identical to events, with one exception: in the current implementation a signal will push it's current value to a new subscriber immediately (kind of like a `ReplaySubject(1)`).

```purescript
subscribe :: forall b a. Signal a -> (a -> Effect b) -> Effect (Effect Unit)
```

### Extra functionality

- `distinct :: forall a. Eq a => Signal a -> Signal a`
- `event :: forall a. Signal a -> Event a`
- `share :: forall a. Signal a -> Effect (Signal a)`
