{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, PatternSynonyms, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Gen
( module Control.Carrier.Pure
  -- * Polymorphic generation & instantiation
, m
, genT
, a
, b
, e
, r
, s
, w
, T(..)
, A
, B
, E
, R
, S
, W
  -- * Generation
, Rec(..)
, forall
  -- * Showing generated values
, With(labelWith, getWith)
, showing
, showingFn
, atom
, liftWith
, liftWith2
, liftWith2InfixL
, liftWith2InfixR
, addLabel
, labelling
  -- * Pattern synonyms
, pattern With
, pattern Fn
, pattern FnWith
  -- * Re-exports
, MonadGen(GenBase)
, Gen
, Identity
, (===)
, (/==)
, choice
, subterm
, subtermM
, subterm2
, Arg
, Vary
, Gen.fn
, apply
, WithT(..)
) where

import Control.Carrier.Pure
import Control.Monad.Morph
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Identity
import Data.Proxy
import qualified Data.Semigroup as S
import qualified Data.Set as Set
import Data.String (fromString)
import GHC.Stack
import GHC.TypeLits
import Hedgehog
import Hedgehog.Function as Fn hiding (R, S)
import Hedgehog.Gen
import Hedgehog.Internal.Distributive
import Hedgehog.Range

-- | A generator for computations, given a higher-order generator for effectful operations, & a generator for results.
m
  :: forall m a
  .  (Monad m, Show a)
  => (forall a . Show a => (forall a . Show a => Gen a -> Gen (With (m a))) -> Gen a -> Gen (With (m a))) -- ^ A higher-order generator producing operations using any effects in @m@.
  -> Gen a                                                                                                -- ^ A generator for results.
  -> Gen (With (m a))                                                                                     -- ^ A generator producing computations, wrapped in 'With' for convenience.
m with = go where
  go :: forall a . Show a => Gen a -> Gen (With (m a))
  go a = recursive choice
    [ addLabel "pure" . liftWith "pure" pure . showing <$> a ]
    [ frequency
      [ (3, with go a)
      , (1, addLabel ">>" <$> subterm2 (go a) (go a) (liftWith2InfixL 1 ">>" (>>)))
      ]
    ]


genT :: Gen (T a)
genT = T <$> integral (linear 0 100)

newtype T a = T { unT :: Integer }
  deriving (Enum, Eq, Generic, Num, Ord, Real, Vary)

instance Arg (T a)

instance S.Semigroup (T a) where
  T a <> T b = T (a + b)

instance Monoid (T a) where
  mempty = T 0
  mappend = (S.<>)

instance KnownSymbol s => Show (T s) where
  showsPrec d = showsUnaryWith showsPrec (symbolVal (Proxy @s)) d . unT

a :: Gen A
a = genT

type A = T "A"

b :: Gen B
b = genT

type B = T "B"

e :: Gen E
e = genT

type E = T "E"

r :: Gen R
r = genT

type R = T "R"

s :: Gen S
s = genT

type S = T "S"

w :: Gen W
w = genT

type W = T "W"

fn :: (Arg a, Vary a, GenBase m ~ Identity, MonadGen m) => m b -> m (Fn a b)
fn = fromGenT . Fn.fn . toGenT


infixr 5 :.

data Rec as where
  Nil :: Rec '[]
  (:.) :: a -> Rec as -> Rec (a ': as)

forall :: (Forall g f, HasCallStack) => g -> f -> Hedgehog.Property
forall g f = withFrozenCallStack $ Hedgehog.property (forall' g f)

class Forall g f | g -> f, f -> g where
  forall' :: HasCallStack => g -> f -> PropertyT IO ()

instance Forall (Rec '[]) (PropertyT IO ()) where
  forall' Nil = id

instance (Forall (Rec gs) b, Show a) => Forall (Rec (Gen a ': gs)) (a -> b) where
  forall' (g :. gs) f = do
    a <- Hedgehog.forAll g
    forall' gs (f a)


data With a = With' { labelWith :: Set.Set LabelName, _showWith :: Int -> ShowS, getWith :: a }
  deriving (Functor)

instance Eq a => Eq (With a) where
  (==) = (==) `on` getWith

instance Applicative With where
  pure = atom "_"
  With' lf sf f <*> With' la sa a = With' (mappend lf la) (\ d -> showParen (d > 10) (sf 10 . showString " " . sa 11)) (f a)

instance Show (With a) where
  showsPrec d (With' _ s _) = s d

showing :: Show a => a -> With a
showing = With' mempty . flip showsPrec <*> id

showingFn :: (Show a, Show b) => Fn a b -> With (a -> b)
showingFn = With' mempty . flip showsPrec <*> apply

atom :: String -> a -> With a
atom s = With' mempty (\ _ -> showString s)

liftWith :: String -> (a -> b) -> With a -> With b
liftWith s w a = atom s w <*> a

liftWith2 :: String -> (a -> b -> c) -> With a -> With b -> With c
liftWith2 s w a b = atom s w <*> a <*> b

liftWith2InfixL :: Int -> String -> (a -> b -> c) -> With a -> With b -> With c
liftWith2InfixL p s f (With' la sa a) (With' lb sb b) = With' (mappend la lb) (\ d -> showParen (d > p) (sa p . showString " " . showString s . showString " " . sb (succ p))) (f a b)

liftWith2InfixR :: Int -> String -> (a -> b -> c) -> With a -> With b -> With c
liftWith2InfixR p s f (With' la sa a) (With' lb sb b) = With' (mappend la lb) (\ d -> showParen (d > p) (sa (succ p) . showString " " . showString s . showString " " . sb p)) (f a b)

addLabel :: String -> With a -> With a
addLabel s w = w { labelWith = Set.insert (fromString s) (labelWith w) }

labelling :: (MonadTest m, HasCallStack) => With a -> m ()
labelling = withFrozenCallStack . traverse_ label . labelWith


pattern With :: a -> With a
pattern With a <- (With' _ _ a)

{-# COMPLETE With #-}

pattern Fn :: (a -> b) -> Fn a b
pattern Fn f <- (apply -> f)

{-# COMPLETE Fn #-}

pattern FnWith :: (a -> b) -> Fn a (With b)
pattern FnWith f <- (fmap getWith . apply -> f)

{-# COMPLETE FnWith #-}


newtype WithT m a = WithT { runWithT :: m (With a) }
  deriving (Functor)

instance Applicative m => Applicative (WithT m) where
  pure = WithT . pure . pure
  WithT m1 <*> WithT m2 = WithT ((<*>) <$> m1 <*> m2)

instance Monad m => Monad (WithT m) where
  WithT m >>= f = WithT $ do
    With' l1 _  a <- m
    With' l2 s2 b <- runWithT (f a)
    pure (With' (l1 <> l2) s2 b)

instance MonadTrans WithT where
  lift = WithT . fmap pure

instance MFunctor WithT where
  hoist f (WithT m) = WithT (f m)

instance MonadTransDistributive WithT where
  distributeT m = lift . WithT . pure =<< hoist lift (runWithT m)

instance MonadGen m => MonadGen (WithT m) where
  type GenBase (WithT m) = WithT (GenBase m)

  toGenT   = distributeT    . hoist toGenT
  fromGenT = hoist fromGenT . distributeT
