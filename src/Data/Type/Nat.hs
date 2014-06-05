{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Nat where

import Data.Functor
import Data.Proxy
import Data.Singletons.TH
import Data.Typeable
import Data.Type.Equality
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Peano-style natural numbers.
--
-- Mainly useful as promoted index.
data Nat = Zero | Suc Nat
  deriving (Show, Read, Eq, Ord, Typeable)

genSingletons [''Nat]

type N0 = Zero
type N1 = Suc N0
type N2 = Suc N1
type N3 = Suc N2
type N4 = Suc N3
type N5 = Suc N4
type N6 = Suc N5

nat :: QuasiQuoter
nat = QuasiQuoter {
        quoteExp  = natExp
      , quotePat  = natPat
      , quoteType = natType
      , quoteDec  = error "cannot quote nat in Dec"
      }
  where
    natExp :: String -> Q Exp
    natExp s = go (read s)
      where
        go :: Integer -> Q Exp
        go 0 = [| Zero |]
        go n = [| Suc $(go (n - 1)) |]

    natPat :: String -> Q Pat
    natPat s = go (read s)
      where
        go :: Integer -> Q Pat
        go 0 = [p| Zero |]
        go n = [p| Suc $(go (n - 1)) |]

    natType :: String -> Q Type
    natType s = go (read s)
      where
        go :: Integer -> Q Type
        go 0 = [t| Zero |]
        go n = [t| Suc $(go (n - 1)) |]

deriving instance Eq (SNat n)
deriving instance Ord (SNat n)
deriving instance Show (SNat n)

snat :: QuasiQuoter
snat = QuasiQuoter {
        quoteExp  = snatExp
      , quotePat  = snatPat
      , quoteType = error "cannot quote snat in Type"
      , quoteDec  = error "cannot quote snat in Dec"
      }
  where
    snatExp :: String -> Q Exp
    snatExp s = go (read s)
      where
        go :: Integer -> Q Exp
        go 0 = [| SZero |]
        go n = [| SSuc $(go (n - 1)) |]

    snatPat :: String -> Q Pat
    snatPat s = go (read s)
      where
        go :: Integer -> Q Pat
        go 0 = [p| SZero |]
        go n = [p| SSuc $(go (n - 1)) |]

type SNatI (n :: Nat) = SingI n

sNat :: SNatI n => SNat n
sNat = sing

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

newtype Shift (r :: Nat -> *) (n :: Nat) where
  Shift :: r (Suc n) -> Shift r n

deriving instance Show (r (Suc n)) => Show (Shift r n)

unShift :: Shift r n -> r (Suc n)
unShift (Shift x) = x

