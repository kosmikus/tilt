{-# LANGUAGE PolyKinds, GADTs #-}
-- | Short names for polykinded basic functors.
module Data.Type.BasicFunctors where

data I a = I a
  deriving Show

data K a b = K a
  deriving Show

newtype Flip (f :: k1 -> k2 -> *) (x :: k2) (y :: k1) where
  Flip :: f y x -> Flip f x y
  deriving Show

unFlip :: Flip f x y -> f y x
unFlip (Flip x) = x
