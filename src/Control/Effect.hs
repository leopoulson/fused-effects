{-# LANGUAGE PolyKinds, TypeOperators #-}
module Control.Effect where

data (f :+: g) (m :: * -> *) a
  = L (f m a)
  | R (g m a)
  deriving (Eq, Ord, Show)
