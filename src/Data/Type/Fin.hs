{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DataKinds #-}
module Data.Type.Fin where

import Data.Type.Nat

data Fin :: Nat -> * where
  FZero :: Fin (Suc n)
  FSuc  :: Fin n -> Fin (Suc n)

deriving instance Show (Fin n)
