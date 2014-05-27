{-# LANGUAGE TypeOperators, KindSignatures, GADTs, DataKinds, PolyKinds, StandaloneDeriving, RankNTypes, RoleAnnotations #-}
module Data.Type.Ptr where

import Data.Type.Some

type role Ptr nominal nominal
data Ptr :: k -> [k] -> * where
  PZero :: Ptr x (x ': xs)
  PSuc  :: Ptr y xs -> Ptr y (x ': xs)

deriving instance Eq (Ptr xs x)
deriving instance Ord (Ptr xs x)
deriving instance Show (Ptr xs x)

shift :: Some Ptr xs -> Some Ptr (x ': xs)
shift sp = withSome sp (Some . PSuc)
