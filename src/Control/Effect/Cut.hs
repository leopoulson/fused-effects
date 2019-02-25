{-# LANGUAGE DeriveFunctor, ExistentialQuantification, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Control.Effect.Cut
( Cut(..)
, cutfail
, call
, cut
, runCut
, CutC(..)
) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Effect.Carrier
import Control.Effect.NonDet
import Control.Effect.Sum

-- | 'Cut' effects are used with 'NonDet' to provide control over backtracking.
data Cut m k
  = Cutfail
  | forall a . Call (m a) (a -> k)

deriving instance Functor (Cut m)

instance HFunctor Cut where
  hmap _ Cutfail    = Cutfail
  hmap f (Call m k) = Call (f m) k
  {-# INLINE hmap #-}

instance Effect Cut where
  handle _     _       Cutfail    = Cutfail
  handle state handler (Call m k) = Call (handler (m <$ state)) (handler . fmap k)
  {-# INLINE handle #-}

-- | Fail the current branch, and prevent backtracking within the nearest enclosing 'call' (if any).
--
--   Contrast with 'empty', which fails the current branch but allows backtracking.
--
--   prop> run (runNonDet (runCut (cutfail <|> pure a))) == []
--   prop> run (runNonDet (runCut (pure a <|> cutfail))) == [a]
cutfail :: (Carrier sig m, Member Cut sig) => m a
cutfail = send Cutfail
{-# INLINE cutfail #-}

-- | Delimit the effect of 'cutfail's, allowing backtracking to resume.
--
--   prop> run (runNonDet (runCut (call (cutfail <|> pure a) <|> pure b))) == [b]
call :: (Carrier sig m, Member Cut sig) => m a -> m a
call m = send (Call m ret)
{-# INLINE call #-}

-- | Commit to the current branch, preventing backtracking within the nearest enclosing 'call' (if any) on failure.
--
--   prop> run (runNonDet (runCut (pure a <|> cut *> pure b))) == [a, b]
--   prop> run (runNonDet (runCut (cut *> pure a <|> pure b))) == [a]
--   prop> run (runNonDet (runCut (cut *> empty <|> pure a))) == []
cut :: (Alternative m, Carrier sig m, Member Cut sig) => m ()
cut = pure () <|> cutfail
{-# INLINE cut #-}


-- | Run a 'Cut' effect within an underlying 'Alternative' instance (typically another 'Carrier' for a 'NonDet' effect).
--
--   prop> run (runNonDetOnce (runCut (pure a))) == Just a
runCut :: (Alternative m, Carrier sig m) => CutC m a -> m a
runCut = (>>= runBranch (const empty)) . runCutC

newtype CutC m a = CutC { runCutC :: m (Branch m Bool a) }
  deriving (Functor)

instance Alternative m => Applicative (CutC m) where
  pure = CutC . pure . Pure
  CutC f <*> CutC a = CutC (liftA2 (<*>) f a)

instance (Alternative m, Carrier sig m, Effect sig) => Alternative (CutC m) where
  empty = send Empty
  l <|> r = send (Choose (\ c -> if c then l else r))

instance (Alternative m, Monad m) => Monad (CutC m) where
  CutC m >>= f = CutC (m >>= \case
    None e    -> pure (None e)
    Pure a    -> runCutC (f a)
    Alt m1 m2 -> let k = runCutC . f in (m1 >>= k) <|> (m2 >>= k))

instance (Alternative m, Carrier sig m, Effect sig) => Carrier (Cut :+: NonDet :+: sig) (CutC m) where
  eff = CutC . handleSum (handleSum
    (eff . handle (Pure ()) (bindBranch (ret (None False)) runCutC))
    (\case
      Empty    -> ret (None True)
      Choose k -> runCutC (k True) >>= branch (\ e -> if e then runCutC (k False) else ret (None False)) (\ a -> ret (Alt (ret a) (runCutC (k False) >>= runBranch (const empty)))) (fmap ret . Alt)))
    (\case
      Cutfail  -> ret (None False)
      Call m k -> runCutC m >>= bindBranch (ret (None True)) (runCutC . k))
    where bindBranch :: (Alternative m, Carrier sig m) => m (Branch m Bool a) -> (b -> m (Branch m Bool a)) -> Branch m Bool b -> m (Branch m Bool a)
          bindBranch cut bind = branch (\ e -> if e then ret (None True) else cut) bind (\ a b -> ret (Alt (a >>= bind >>= runBranch (const empty)) (b >>= bind >>= runBranch (const empty))))
  {-# INLINE eff #-}


-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> import Test.QuickCheck
-- >>> import Control.Effect.Void
