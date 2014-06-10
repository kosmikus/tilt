-- | Short names for polykinded basic functors.

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Type.BasicFunctors where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.Typeable

-- | The identity functor.
type role I representational
newtype I (a :: *) = I a
  deriving ( Eq, Ord, Show, Read, Typeable
           , Functor, Foldable, Traversable, Monoid
           )

-- | Extract a value from the identity functor.
unI :: I a -> a
unI (I x) = x

instance Applicative I where
  pure  = return
  (<*>) = ap

instance Monad I where
  return = I
  I x >>= f = f x

-- | The polykinded constant functor.
type role K representational phantom
newtype K (a :: *) (b :: k) = K a
  deriving ( Eq, Ord, Show, Read, Typeable
           , Functor, Foldable, Traversable, Monoid
           )

-- | Extract a value from the constant functor.
unK :: K a b -> a
unK (K x) = x

instance Monoid a => Applicative (K a) where
  pure x      = K mempty
  K x <*> K y = K (x <> y)

-- | Type-level version of 'flip'.
--
-- Take a binary type constructor and flips the order in which it
-- expects its arguments.
type role Flip representational nominal nominal
newtype Flip (f :: k1 -> k2 -> *) (x :: k2) (y :: k1) where
  Flip :: f y x -> Flip f x y
  deriving (Eq, Ord, Show, Read, Typeable, Monoid)

-- | Extract a value from 'Flip'.
unFlip :: Flip f x y -> f y x
unFlip (Flip x) = x
