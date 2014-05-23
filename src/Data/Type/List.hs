{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
-- | Functionality on promoted lists.
module Data.Type.List where

type family Map (f :: k -> l) (xs :: [k]) :: [l]
type instance Map f '[]       = '[]
type instance Map f (x ': xs) = f x ': Map f xs

type family ReverseAcc (xs :: [k]) (acc :: [k]) :: [k]
type instance ReverseAcc '[]       acc = acc
type instance ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

type Reverse (xs :: [k]) = ReverseAcc xs '[]

type family (++) (xs :: [k]) (ys :: [k]) :: [k]
type instance (++) '[]       ys = ys
type instance (++) (x ': xs) ys = x ': (xs ++ ys)
