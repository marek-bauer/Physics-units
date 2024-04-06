{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}

module GeneralizedOperations.Neg 
  ( Neg(..)
  , (GeneralizedOperations.Neg.-) 
  )
where

import Prelude hiding ((+), (-))
import GeneralizedOperations.Add ( Add(..) )

-- | Class of negatable types.
class Neg a b | a -> b where
  neg :: a -> b

instance {-# OVERLAPPABLE #-} (Num a, a ~ b) => Neg a b where
  neg = neg

(-) :: (Add a b' c, Neg b b') => a -> b -> c
x - y = x + neg y
