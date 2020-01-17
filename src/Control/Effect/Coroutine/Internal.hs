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
  hmap f (Yield x res) = Yield x (f . res)
