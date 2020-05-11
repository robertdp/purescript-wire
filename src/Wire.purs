module Wire
  ( module Class
  , module Signal
  ) where

import Wire.Class (class Readable, class Writable, read, subscribe, subscribe', write) as Class
import Wire.Signal (Signal(..), create, readOnly) as Signal
