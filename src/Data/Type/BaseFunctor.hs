{-# LANGUAGE PolyKinds #-}
-- | Short names for polykinded basic functors.
module Data.Type.BaseFunctor where

data I a = I a
  deriving Show

data K a b = K a
  deriving Show

