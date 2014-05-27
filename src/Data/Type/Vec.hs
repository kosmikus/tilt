{-# LANGUAGE GADTs, KindSignatures, DataKinds, StandaloneDeriving, InstanceSigs, TypeFamilies, TypeOperators #-}
module Data.Type.Vec where

import Data.Type.Equality
import Data.Type.Fin
import Data.Type.Nat
import Prelude hiding (length, replicate, zipWith, (++), (!!))

data Vec :: Nat -> * -> * where
  Nil  :: Vec Zero a
  (:*) :: a -> Vec n a -> Vec (Suc n) a

deriving instance (Show a) => Show (Vec n a)

infixr 5 :*

length :: Vec n a -> SNat n
length Nil       = SZero
length (x :* xs) = SSuc (length xs)

toList :: Vec n a -> [a]
toList Nil       = []
toList (x :* xs) = x : toList xs

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith op Nil       Nil       = Nil
zipWith op (x :* xs) (y :* ys) = op x y :* zipWith op xs ys

instance Functor (Vec n) where
  fmap :: (a -> b) -> Vec n a -> Vec n b
  fmap f Nil       = Nil
  fmap f (x :* xs) = f x :* fmap f xs

replicate :: SNat n -> a -> Vec n a
replicate SZero    x = Nil
replicate (SSuc n) x = x :* replicate n x

replicate' :: SNatI n => a -> Vec n a
replicate' = replicate sNat

(++) :: Vec m a -> Vec n a -> Vec (m + n) a
Nil       ++ ys = ys
(x :* xs) ++ ys = x :* (xs ++ ys)

reverse :: Vec n a -> Vec n a
reverse xs = gcastWith (thmPlusZero (length xs)) $ go xs Nil
  where
    go :: Vec p a -> Vec q a -> Vec (p + q) a
    go Nil       acc = acc
    go (x :* xs) acc =
      gcastWith (thmPlusSuc (length xs) (length acc)) $
      go xs (x :* acc)

(!!) :: Vec n a -> Fin n -> a
(x :* xs) !! FZero  = x
(x :* xs) !! FSuc i = xs !! i