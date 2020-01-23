{-# LANGUAGE DeriveFunctor, FlexibleInstances, KindSignatures, MultiParamTypeClasses, UndecidableInstances #-}

module Control.Effect.Coroutine.Internal
  ( Yield(..)
  , Status(..)
  ) where

import Control.Algebra
-- import Control.Effect.Class
import Control.Monad (liftM2)

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
    deriving (Functor)

instance Monad m => Applicative (Status m a b) where
  pure = Done
  Done f <*> Done x = Done (f x)
  Done f <*> Continue x k = Continue x (\b -> fmap f <$> k b)
  Continue x k <*> Done y = Continue x (\b -> fmap ($ y) <$> k b)
  -- What is the best thing to fill the hole? Is it y or x?
  Continue _ k <*> Continue y k' = Continue y (\b -> liftM2 (<*>) (k b) (k' b))

-- >>= :: m a -> (a -> m b) -> m b
instance Monad m => Monad (Status m a b) where
  return = pure
  Done a >>= f = f a
  Continue x k >>= f = Continue x (fmap (>>= f) . k)

-- instance (Algebra sig m) => Algebra sig (Status m a b) where
--   alg ((Done _)) = undefined
--   alg (Continue x k) = undefined
--   -- alg (L (Continue x k)) = undefined
--   -- alg (R other) = undefined
