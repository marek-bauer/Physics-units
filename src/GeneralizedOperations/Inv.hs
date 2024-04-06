{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}

module GeneralizedOperations.Inv 
  ( Inv(..)
  , (GeneralizedOperations.Inv./) 
  )
where

import Prelude hiding ((*))
import GeneralizedOperations.Mul ( Mul(..) )

-- | Class of invertible types.

class Inv a b | a -> b where
  inv :: a -> b

instance {-# OVERLAPPABLE #-} (Fractional a, a ~ b) => Inv a b where
  inv = recip

(/) :: (Mul a b' c, Inv b b') => a -> b -> c
x / y = x * inv y