{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}

module GeneralizedOperations.Add 
  ( Add(..) )
where

import Prelude

-- | Class of generalized addable types.
class Add a b c | a b -> c where
  (+) :: a -> b -> c

instance {-# OVERLAPPABLE #-} (Num a, a ~ b, b ~ c) => Add a b c where
  x + y = x Prelude.+ y

-- instance (Add a b c) => Add b a c where
--   x + y = y GeneralizedOperations.Add.+ x