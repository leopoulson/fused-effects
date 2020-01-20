{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Control.Carrier.Coroutine.Lazy
( YieldC(..)

  )where

-- import Control.Algebra
import Control.Effect.Coroutine

newtype YieldC a b m c = YieldC { unYieldC :: Status m a b c }
  deriving (Functor)

instance Monad m => Applicative (YieldC a b m) where
  pure a = YieldC (pure a)
  YieldC f <*> YieldC a = YieldC (f <*> a)

instance Monad m => Monad (YieldC a b m) where
  return = pure
  YieldC (Done x) >>= f = f x
  YieldC (Continue x k) >>= f =
    YieldC (Continue x (fmap (unYieldC . (>>= f) . YieldC) . k))

-- alg :: sig m a -> m a
-- So we need to produce something of type YieldC a b m c
-- What things are of this type?
-- It's Status things, I think
-- instance (Algebra sig m) => Algebra (Yield a b :+: sig) (YieldC a b m) where -
--   alg (L (Yield x k)) = undefined
--   alg (R y) = undefined
