{-# LANGUAGE TemplateHaskell #-}
module Data.Type.Ptr where

import Data.Typeable
import Data.Type.Some
import Language.Haskell.TH
import Language.Haskell.TH.Quote

type role Ptr nominal nominal
data Ptr :: k -> [k] -> * where
  PZero :: Ptr x (x ': xs)
  PSuc  :: Ptr y xs -> Ptr y (x ': xs)

deriving instance Eq (Ptr xs x)
deriving instance Ord (Ptr xs x)
deriving instance Show (Ptr xs x)
deriving instance Typeable Ptr

ptr :: QuasiQuoter
ptr = QuasiQuoter {
        quoteExp  = ptrExp
      , quotePat  = ptrPat
      , quoteType = error "cannot quote ptr in Type"
      , quoteDec  = error "cannot quote ptr in Dec"
      }
  where
    ptrExp :: String -> Q Exp
    ptrExp s = go (read s)
      where
        go :: Integer -> Q Exp
        go 0 = [| PZero |]
        go n = [| PSuc $(go (n - 1)) |]

    ptrPat :: String -> Q Pat
    ptrPat s = go (read s)
      where
        go :: Integer -> Q Pat
        go 0 = [p| PZero |]
        go n = [p| PSuc $(go (n - 1)) |]

shift :: Some Ptr xs -> Some Ptr (x ': xs)
shift sp = withSome sp (Some . PSuc)
