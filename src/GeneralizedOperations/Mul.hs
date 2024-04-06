{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module GeneralizedOperations.Mul 
  ( Mul(..) )
where

import Prelude

-- | Class of generalized multiplicative types.
class Mul a b c | a b -> c where
  (*) :: a -> b -> c

instance {-# OVERLAPPABLE #-} (Num a, a ~ b, b ~ c) => Mul a b c where
  x * y = x Prelude.* y