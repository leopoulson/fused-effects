{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}

module Control.Effect.Coroutine.Internal
  ( Yield(..)
  ) where

import Control.Effect.Class

-- This is the datatype of computations that yield control.
data Yield a b (m :: * -> *) c = Yield a (b -> m c)
  deriving (Functor)

-- This is an effect type, so it must implement 'HFunctor' and 'Effect'
-- TODO: Understand this better
instance HFunctor (Yield a b) where

  -- hmap :: m x -> n x -> h m a -> h n a
  hmap f (Yield x res) = Yield x (f . res)

instance Effect (Yield a b) where
  thread ctx handler (Yield x res) = Yield x (handler . (<$ ctx) . res)

-- This datatype represents the status of a coroutine.
data Status m a b r
  = Done r
--  ^ Coroutine is done, returning a value of type `r`.
  | Continue a (b -> m (Status m a b r))
--  ^ Coroutine is not done.
--  Reports a value of type `a`, being the computation thus far
--  Resumes with type b, possibly returns a value of `m (Status m a b r)`.
