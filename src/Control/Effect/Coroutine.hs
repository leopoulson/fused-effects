-- |

module Control.Effect.Coroutine
(  Yield(..)
  , yield

  ) where

import Control.Algebra
import Control.Effect.Coroutine.Internal (Yield(..))

-- In here we need to define useful corountine related functions
-- TODO: yield
-- TODO:

yield :: Has (Yield a b) sig m => a -> (b -> c) -> m c
yield x f = send (Yield x (pure . f))
