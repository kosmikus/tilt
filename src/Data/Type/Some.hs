module Data.Type.Some where

import Data.Typeable

data Some :: (k -> k' -> *) -> k' -> * where
  Some :: f b a -> Some f a

deriving instance Typeable Some

withSome :: Some f a -> (forall b. f b a -> r) -> r
withSome (Some x) k = k x
