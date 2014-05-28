{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DataKinds, RoleAnnotations, AutoDeriveTypeable #-}
module Data.Type.Fin where

import Data.Functor
import Data.Type.Nat
import Data.Typeable

type role Fin nominal
data Fin :: Nat -> * where
  FZero :: Fin (Suc n)
  FSuc  :: Fin n -> Fin (Suc n)

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)
deriving instance Show (Fin n)
deriving instance Typeable Fin

toNat :: Fin n -> Nat
toNat FZero    = Zero
toNat (FSuc i) = Suc (toNat i)

