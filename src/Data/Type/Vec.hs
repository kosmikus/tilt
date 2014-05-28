{-# LANGUAGE GADTs, KindSignatures, DataKinds, StandaloneDeriving, InstanceSigs, TypeFamilies, TypeOperators, RoleAnnotations, RankNTypes, ScopedTypeVariables #-}
module Data.Type.Vec where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Type.Equality
import Data.Type.Fin
import Data.Type.Nat
import Prelude hiding (length, replicate, zipWith, (++), (!!))

type role Vec nominal representational
data Vec :: Nat -> * -> * where
  Nil  :: Vec Zero a
  (:*) :: a -> Vec n a -> Vec (Suc n) a

infixr 5 :*

deriving instance Eq a => Eq (Vec n a)
deriving instance Ord a => Ord (Vec n a)
deriving instance Show a => Show (Vec n a)

head :: Vec (Suc n) a -> a
head (x :* xs) = x

tail :: Vec (Suc n) a -> Vec n a
tail (x :* xs) = xs

length :: Vec n a -> SNat n
length Nil       = SZero
length (x :* xs) = SSuc (length xs)

instance Foldable (Vec n) where
  foldMap :: (Monoid m) => (a -> m) -> Vec n a -> m
  foldMap f Nil       = mempty
  foldMap f (x :* xs) = f x <> foldMap f xs

zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith op Nil       Nil       = Nil
zipWith op (x :* xs) (y :* ys) = op x y :* zipWith op xs ys

cata :: forall a r n.
        r Zero
     -> (forall n. a -> r n -> r (Suc n))
     -> Vec n a
     -> r n
cata nil cons = go
  where
    go :: forall n. Vec n a -> r n
    go Nil       = nil
    go (x :* xs) = cons x (go xs)

instance Functor (Vec n) where
  fmap :: (a -> b) -> Vec n a -> Vec n b
  fmap f Nil       = Nil
  fmap f (x :* xs) = f x :* fmap f xs

instance Traversable (Vec n) where
  traverse :: Applicative i => (a -> i b) -> Vec n a -> i (Vec n b)
  traverse f Nil       = pure Nil
  traverse f (x :* xs) = (:*) <$> f x <*> traverse f xs

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

