{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DataKinds, RoleAnnotations #-}
module Data.Type.Fin where

import Data.Type.Nat

type role Fin nominal
data Fin :: Nat -> * where
  FZero :: Fin (Suc n)
  FSuc  :: Fin n -> Fin (Suc n)

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)
deriving instance Show (Fin n)
