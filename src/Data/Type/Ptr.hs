{-# LANGUAGE TypeOperators, KindSignatures, GADTs, DataKinds, PolyKinds, StandaloneDeriving, RankNTypes #-}
module Data.Type.Ptr where

data Ptr :: [k] -> k -> * where
  PZero :: Ptr (x ': xs) x
  PSuc  :: Ptr xs y -> Ptr (x ': xs) y

deriving instance Show (Ptr xs x)

data SomePtr :: [k] -> * where
  SomePtr :: Ptr xs x -> SomePtr xs

withSomePtr :: SomePtr xs -> (forall x. Ptr xs x -> r) -> r
withSomePtr (SomePtr xs) k = k xs

shift :: SomePtr xs -> SomePtr (x ': xs)
shift sp = withSomePtr sp (SomePtr . PSuc)
