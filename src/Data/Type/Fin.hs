{-# LANGUAGE TemplateHaskell #-}
module Data.Type.Fin where

import Data.Functor
import Data.Singletons.TH
import Data.Type.Nat
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Quote

type role Fin nominal
data Fin :: Nat -> * where
  FZero :: Fin (Suc n)
  FSuc  :: Fin n -> Fin (Suc n)

deriving instance Eq (Fin n)
deriving instance Ord (Fin n)
deriving instance Show (Fin n)
deriving instance Typeable Fin

fin :: QuasiQuoter
fin = QuasiQuoter {
        quoteExp  = finExp
      , quotePat  = finPat
      , quoteType = error "cannot quote fin in Type"
      , quoteDec  = error "cannot quote fin in Dec"
      }
  where
    finExp :: String -> Q Exp
    finExp s = go (read s)
      where
        go :: Integer -> Q Exp
        go 0 = [| FZero |]
        go n = [| FSuc $(go (n - 1)) |]

    finPat :: String -> Q Pat
    finPat s = go (read s)
      where
        go :: Integer -> Q Pat
        go 0 = [p| FZero |]
        go n = [p| FSuc $(go (n - 1)) |]

toNat :: Fin n -> Nat
toNat FZero    = Zero
toNat (FSuc i) = Suc (toNat i)

instance SNatI n => Bounded (Fin (Suc n)) where
  minBound = FZero
  maxBound = maxBound' sNat

maxBound' :: SNat n -> Fin (Suc n)
maxBound' SZero    = FZero
maxBound' (SSuc s) = FSuc (maxBound' s)
