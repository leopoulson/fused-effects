-- |

module Control.Effect.Coroutine
(  Yield(..)
  , yield
  , Status(..)

  ) where

import Control.Algebra
import Control.Effect.Coroutine.Internal (Yield(..), Status(..))


yield :: Has (Yield a b) sig m => a -> (b -> c) -> m c
yield x f = send (Yield x (pure . f))
