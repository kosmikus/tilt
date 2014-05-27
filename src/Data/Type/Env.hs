{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures, TypeOperators, DataKinds, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, ExplicitForAll, RankNTypes, ConstraintKinds, TypeFamilies, PolyKinds, UndecidableInstances, ScopedTypeVariables, RoleAnnotations #-}
module Data.Type.Env where

import Control.Applicative
import Data.Functor ((<$>))
import qualified Data.List as L
import Data.Type.BasicFunctors
import Data.Type.Constraint
import Data.Type.List
import Data.Type.Ptr
import Data.Type.Some
import GHC.Exts (Constraint)
import Prelude hiding (reverse, zipWith, map, (!!), (++))

type role Env nominal representational
data Env :: [k] -> (k -> *) -> * where
  Nil  :: Env '[] f
  (:*) :: f a -> Env as f -> Env (a ': as) f

infixr 5 :*

deriving instance All Eq (Map f as) => Eq (Env as f)
deriving instance (All Eq (Map f as), All Ord (Map f as)) => Ord (Env as f)
deriving instance All Show (Map f as) => Show (Env as f)

map :: (forall a. f a -> g a)
    -> Env xs f -> Env xs g
map f Nil       = Nil
map f (x :* xs) = f x :* map f xs

traverse :: Applicative i
         => (forall a. f a -> i (g a))
         -> Env xs f -> i (Env xs g)
traverse f Nil = pure Nil
traverse f (x :* xs) = (:*) <$> f x <*> traverse f xs

zipWith :: (forall a. f a -> g a -> h a)
        -> Env xs f -> Env xs g -> Env xs h
zipWith op Nil       Nil       = Nil
zipWith op (x :* xs) (y :* ys) = op x y :* zipWith op xs ys

foldr :: forall f r xs.
         (forall x xs. f x -> r xs -> r (x ': xs))
      -> r '[]
      -> Env xs f
      -> r xs
foldr op e = go
  where
    go :: forall xs. Env xs f -> r xs
    go Nil       = e
    go (x :* xs) = op x (go xs)

toList :: Env xs (K a) -> [a]
toList Nil         = []
toList (K x :* xs) = x : toList xs

(!!) :: Env xs f -> Ptr x xs -> f x
(x :* xs) !! PZero  = x
(x :* xs) !! PSuc i = xs !! i

(++) :: Env xs f -> Env ys f -> Env (xs ++ ys) f
Nil       ++ ys = ys
(x :* xs) ++ ys = x :* xs ++ ys

infixr 5 ++

elemPtr :: (forall a. f a -> Bool)
        -> Env as f
        -> Maybe (Some Ptr as)
elemPtr p Nil       = Nothing
elemPtr p (x :* xs)
  | p x             = Just (Some PZero)
  | otherwise       = shift <$> elemPtr p xs

elemPtrs :: (forall a. f a -> Bool)
         -> Env as f
         -> [Some Ptr as]
elemPtrs p Nil       = []
elemPtrs p (x :* xs)
  | p x              = Some PZero : L.map shift (elemPtrs p xs)
  | otherwise        = L.map shift (elemPtrs p xs)

reverse :: Env as f -> Env (Reverse as) f
reverse xs = go xs Nil
  where
    go :: Env as f -> Env bs f -> Env (ReverseAcc as bs) f
    go Nil       acc = acc
    go (x :* xs) acc = go xs (x :* acc)

