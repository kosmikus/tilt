{-# LANGUAGE TypeOperators, KindSignatures, GADTs, DataKinds, PolyKinds, StandaloneDeriving #-}
module Data.Type.Ptr where

data Ptr :: [k] -> k -> * where
  PZero :: Ptr (x ': xs) x
  PSuc  :: Ptr xs y -> Ptr (x ': xs) y

deriving instance Show (Ptr xs x)

data SomePtr :: [k] -> * where
  SomePtr :: Ptr xs x -> SomePtr xs
