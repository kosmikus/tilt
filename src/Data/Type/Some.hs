{-# LANGUAGE PolyKinds, GADTs, KindSignatures, RankNTypes #-}
module Data.Type.Some where

data Some :: (k -> k' -> *) -> k' -> * where
  Some :: f b a -> Some f a

withSome :: Some f a -> (forall b. f b a -> r) -> r
withSome (Some x) k = k x
