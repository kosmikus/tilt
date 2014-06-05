module Data.Type.Constraint (module Data.Type.Constraint, module GHC.Exts) where

import Data.Singletons (TyCon1)
import Data.Type.List
import GHC.Exts (Constraint)

type family Constraints (cs :: [Constraint]) :: Constraint
type instance Constraints '[]       = ()
type instance Constraints (c ': cs) = (c, Constraints cs)

type All (c :: k -> Constraint) (xs :: [k]) = Constraints (Map (TyCon1 c) xs)
type AllF (c :: k -> Constraint) (f :: l -> k) (xs :: [l]) = Constraints (Map (TyCon1 c) (Map (TyCon1 f) xs))
