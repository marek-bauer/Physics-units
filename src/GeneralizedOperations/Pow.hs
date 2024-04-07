{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeneralizedOperations.Pow 
  ( P
  , ConstPower(..)
  , ConstPow(..)
  , root
  , sqrt
  , sq
  )
where

import Prelude hiding (sqrt)
import TypeNumbers.Rational (QPos, QPos', TRational)
import GHC.TypeLits (Nat)

data P (a :: k) = P

class ConstPower a (q :: TRational) b | a q -> b where
  power :: p q -> a -> b

class ConstPow a (n :: Nat) b | a n -> b where
  pow :: p n -> a -> b

root :: forall (n :: Nat) a b p. ConstPower a (QPos 1 n) b => p n -> a -> b
root _ = power (P :: P (QPos 1 n))

sqrt :: ConstPower a (QPos 1 2) b => a -> b
sqrt = root (P :: P 2)

sq :: ConstPow a 2 b => a -> b
sq = pow (P :: P 2)