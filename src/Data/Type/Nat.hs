-- | Peano-style natural numbers.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Nat where

import Data.Functor
import Data.Proxy
import Data.Singletons.TH
import Data.Typeable
import Data.Type.Equality
import GHC.Exts (Constraint)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Peano-style natural numbers.
--
-- Mainly useful as promoted index.
--
-- There's a 'QuasiQuoter' that allows you to write down natural number
-- literals in expressions, patterns and types (promoted). Example:
--
-- >>> [nat| 3 |]
-- Suc (Suc (Suc Zero))
-- >>> :t [nat| 3 |]
-- [nat| 3 |] :: Nat
-- >>> :k [nat| 3 |]
-- [nat| 3 |] :: Nat
--
data Nat = Zero | Suc Nat
  deriving (Show, Read, Eq, Ord, Typeable)

genSingletons [''Nat]
singEqInstance ''Nat
singDecideInstance ''Nat

type N0  = Zero
type N1  = Suc N0
type N2  = Suc N1
type N3  = Suc N2
type N4  = Suc N3
type N5  = Suc N4
type N6  = Suc N5
type N7  = Suc N6
type N8  = Suc N7
type N9  = Suc N8
type N10 = Suc N9

-- | A 'QuasiQuoter' for literals of type 'Nat'.
--
-- See the documentation of 'Nat' for further details.
--
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

-- | A 'QuasiQuoter' for literals of type 'SNat'.
--
-- This quasiquoter can be used in expressions and in patterns.
--
-- Examples:
--
-- >>> [snat| 3 |]
-- SSuc (SSuc (SSuc SZero))
-- >>> :t [snat| 3 |]
-- [snat| 3 |] :: Sing ('Suc ('Suc ('Suc 'Zero)))
-- >>> :t [snat| 2 |]
-- [snat| 2 |] :: Sing ('Suc ('Suc 'Zero))
--
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

-- | Kind-specialized synonym for 'SingI'.
type SNatI = SNatI' SingI
type SNatI' (n :: Nat -> Constraint) = n

-- | Type-specialized synonym for 'sing'.
sNat :: SNatI n => SNat n
sNat = sing

-- | General induction principle for 'Nat'-indexed types.
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

-- | Standard type-level addition on natural numbers.
--
-- > Zero  + n = n
-- > Suc m + n = Suc (m + n)
--
type family (+) (m :: Nat) (n :: Nat) :: Nat
type instance (+) Zero    n = n
type instance (+) (Suc m) n = Suc (m + n)

-- | "Accumulating" type-level addition on natural numbers.
--
-- > Zero  + n = n
-- > Suc m + n = n + Suc m
--
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

