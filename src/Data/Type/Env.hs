{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures, TypeOperators, DataKinds, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, ExplicitForAll, RankNTypes, ConstraintKinds, TypeFamilies, PolyKinds, UndecidableInstances #-}
module Data.Type.Env where

import Data.Functor ((<$>))
import Data.Type.BaseFunctor
import Data.Type.Constraint
import Data.Type.List
import Data.Type.Ptr
import GHC.Exts (Constraint)
import Prelude hiding (reverse, zipWith, (!!))

data Env :: [k] -> (k -> *) -> * where
  Nil  :: Env '[] f
  (:*) :: f a -> Env as f -> Env (a ': as) f

deriving instance All Show (Map f as) => Show (Env as f)

infixr 5 :*

zipWith :: (forall a. f a -> g a -> h a)
        -> Env xs f -> Env xs g -> Env xs h
zipWith op Nil       Nil       = Nil
zipWith op (x :* xs) (y :* ys) = op x y :* zipWith op xs ys

toList :: Env xs (K a) -> [a]
toList Nil         = []
toList (K x :* xs) = x : toList xs

(!!) :: Env xs f -> Ptr xs x -> f x
(x :* xs) !! PZero  = x
(x :* xs) !! PSuc i = xs !! i

findPtr :: (forall a. f a -> Bool)
        -> Env as f
        -> Maybe (SomePtr as)
findPtr p Nil       = Nothing
findPtr p (x :* xs)
  | p x             = Just (SomePtr PZero)
  | otherwise       = (\ (SomePtr i) -> SomePtr (PSuc i)) <$> findPtr p xs

reverse :: Env as f -> Env (Reverse as) f
reverse xs = go xs Nil
  where
    go :: Env as f -> Env bs f -> Env (ReverseAcc as bs) f
    go Nil       acc = acc
    go (x :* xs) acc = go xs (x :* acc)

data SomeEnv :: (k -> *) -> * where
  SomeEnv :: Env as f -> SomeEnv f

withSomeEnv :: SomeEnv f -> (forall as. Env as f -> r) -> r
withSomeEnv (SomeEnv xs) k = k xs
