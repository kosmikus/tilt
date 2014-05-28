{-# LANGUAGE DataKinds, GADTs, TypeOperators, KindSignatures, TypeFamilies, UndecidableInstances, RankNTypes, ScopedTypeVariables #-}
module Data.Type.Nat where

import Data.Functor
import Data.Proxy
import Data.Type.Equality

-- | Peano-style natural numbers.
--
-- Mainly useful as promoted index.
data Nat = Zero | Suc Nat
  deriving (Show, Read, Eq, Ord)

type N0 = Zero
type N1 = Suc N0
type N2 = Suc N1
type N3 = Suc N2
type N4 = Suc N3
type N5 = Suc N4
type N6 = Suc N5

data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSuc  :: SNat n -> SNat (Suc n)

class SNatI (n :: Nat) where
  sNat :: SNat n

instance SNatI Zero where
  sNat = SZero

instance SNatI n => SNatI (Suc n) where
  sNat = SSuc sNat

ind :: forall r n.
       r Zero
    -> (forall n. r n -> r (Suc n))
    -> SNat n
    -> r n
ind zero suc = go
  where
    go :: forall n. SNat n -> r n
    go SZero    = zero
    go (SSuc n) = suc (go n)

(==?) :: SNat m -> SNat n -> Maybe (m :~: n)
(==?) SZero    SZero    = Just Refl
(==?) (SSuc m) (SSuc n) = (\ Refl -> Refl) <$> m ==? n
(==?) _        _        = Nothing

type family (+) (m :: Nat) (n :: Nat) :: Nat
type instance (+) Zero    n = n
type instance (+) (Suc m) n = Suc (m + n)

type family PlusAcc (n :: Nat) (acc :: Nat) :: Nat
type instance PlusAcc Zero    acc = acc
type instance PlusAcc (Suc n) acc = PlusAcc n (Suc acc)

thmPlusZero :: SNat n -> (n + Zero) :~: n
thmPlusZero SZero    = Refl
thmPlusZero (SSuc s) = gcastWith (thmPlusZero s) Refl

thmPlusSuc :: SNat m -> SNat n -> (m + Suc n) :~: (Suc (m + n))
thmPlusSuc SZero    _ = Refl
thmPlusSuc (SSuc s) n = gcastWith (thmPlusSuc s n) Refl
