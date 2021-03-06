{-# LANGUAGE RankNTypes #-}

{- | Provides a mechanism to kick off the evaluation of an effect stack that takes place in a monadic context.

'Lift' effects are always the last effect in a given effect stack. These stacks are invoked with 'Control.Carrier.Lift.runM' or 'Control.Algebra.run'.

Predefined carriers:

* "Control.Carrier.Lift"
* 'IO'
* 'Data.Functor.Identity.Identity'

@since 0.1.0.0
-}

module Control.Effect.Lift
( -- * Lift effect
  Lift(..)
, sendM
, liftWith
  -- * Re-exports
, Algebra
, Effect
, Has
, run
) where

import Control.Algebra
import Control.Effect.Lift.Internal (Lift(..))

-- | Given a @Lift n@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
--
-- @since 1.0.0.0
sendM :: (Has (Lift n) sig m, Functor n) => n a -> m a
sendM m = send (LiftWith (\ ctx _ -> (<$ ctx) <$> m) pure)


-- | Run actions in an outer context.
--
-- This can be used to provide interoperation with @base@ functionality like @"Control.Exception".'Control.Exception.catch'@:
--
-- @
-- 'liftWith' $ \ ctx hdl -> 'Control.Exception.catch' (hdl (m <$ ctx)) (hdl . (<$ ctx) . h)
-- @
--
-- The higher-order function takes both an initial context, and a handler phrased as the same sort of distributive law as described in the documentation for 'thread'. This handler takes actions lifted into a context functor, which can be either the initial context, or the derived context produced by handling a previous action.
--
-- As with @MonadBaseControl@, care must be taken when lifting functions like @"Control.Exception".'Control.Exception.finally'@ which don’t use the return value of one of their actions, as this can lead to dropped effects.
--
-- @since 1.0.0.0
liftWith
  :: Has (Lift n) sig m
  => (forall ctx . Functor ctx => ctx () -> (forall a . ctx (m a) -> n (ctx a)) -> n (ctx a))
  -> m a
liftWith with = send (LiftWith with pure)
