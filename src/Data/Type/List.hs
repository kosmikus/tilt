{-# LANGUAGE UndecidableInstances #-}
-- | Functionality on promoted lists.
module Data.Type.List (module Data.Type.List, module Data.Promotion.Prelude.List) where

import Data.Promotion.Prelude.List (Map, (:++))

type family ReverseAcc (xs :: [k]) (acc :: [k]) :: [k]
type instance ReverseAcc '[]       acc = acc
type instance ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

type Reverse (xs :: [k]) = ReverseAcc xs '[]
