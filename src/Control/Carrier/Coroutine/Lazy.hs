{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}

module Control.Carrier.Coroutine.Lazy
( YieldC(..)

  )where

import Control.Algebra
import Control.Effect.Coroutine

newtype YieldC a b m c = YieldC { unYieldC :: Status m a b c }
  deriving (Functor, Applicative)

instance Monad m => Monad (YieldC a b m) where
  return = pure
  YieldC (Done x) >>= f = f x
  YieldC (Continue x k) >>= f =
    YieldC (Continue x (fmap (unYieldC . (>>= f) . YieldC) . k))

-- Taken from Vera's effects
-- This lets us always produce an x, given a status and two functions
-- status :: (c -> x) -> (a -> (b -> m (Status m a b c)) -> x) -> Status m a b c -> x
-- status f _ (Done r) = f r
-- status _ g (Continue x k) = g x k


-- runCoro :: Monad m => (a -> b) -> YieldC a b m c -> m c
-- runCoro _ (YieldC (Done r)) = pure r
-- runCoro f (YieldC (Continue a res)) = (res (f a)) >>= (runCoro f . YieldC)

-- So we need to produce something of type YieldC a b m c
-- What things are of this type?
-- It's Status things, I think
-- alg :: sig m a -> m a
-- here, alg returns something of type YieldC
-- not sure how the types work out here
instance (Algebra sig m, Effect sig) => Algebra (Yield a b :+: sig) (YieldC a b m) where
  alg (L (Yield x k)) = YieldC (Continue x (pure . unYieldC . k))

  -- This is a handler for a Yield effect that may be embedded in another effect
  -- inside the signature.
  -- Perhaps we can use thread here?
  -- alg :: (Yield a b :+: sig) (YieldC a b m) x -> YieldC a b m x

  -- this definition works, but does it do what we want? it will just unwrap the
  -- outer layer of any YieldC and apply the algebra
  -- this is /probably/ wrong
  -- alg (R other) = alg (R (handleCoercible other))
  -- alg (R other) = fmap (status id _) (alg (thread (Done ()) (something) other))
  -- alg (R other) = ((thread (Done ()) _ other))
  -- alg (R other) = (alg (R (other)))
  alg (R other) = alg (R (handleCoercible other))

  -- other :: sig (YieldC a b m)

  -- thread
  --   :: (Functor ctx, Monad m)
  --   => Status ()                              -- ^ The initial context.
  --   -> (forall x . Status (YieldC x) -> n (Status x)) -- ^ A handler for actions in a context, producing actions with a derived context.
  --   -> sig YieldC a                             -- ^ The effect to thread the handler through.
  --   -> sig n (Status a)