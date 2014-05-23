{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, TypeOperators, ConstraintKinds #-}
module Data.Type.Constraint (module Data.Type.Constraint, module GHC.Exts) where

import Data.Type.List
import GHC.Exts (Constraint)

type family Constraints (cs :: [Constraint]) :: Constraint
type instance Constraints '[]       = ()
type instance Constraints (c ': cs) = (c, Constraints cs)

type All (c :: k -> Constraint) (xs :: [k]) = Constraints (Map c xs)
