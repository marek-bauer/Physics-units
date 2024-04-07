module GeneralizedOperations 
  ( Add(..)
  , Neg(..)
  , (-)
  , Mul(..)
  , Inv(..)
  , (/)
  , P
  , ConstPower(..)
  , ConstPow(..)
  , root
  , sqrt
  , sq
  )
where

import Prelude hiding ((+), (-), (*), (/), sqrt)
import GeneralizedOperations.Add (Add(..))
import GeneralizedOperations.Neg (Neg(..), (-))
import GeneralizedOperations.Mul (Mul(..))
import GeneralizedOperations.Inv (Inv(..), (/))
import GeneralizedOperations.Pow (P, ConstPower(..), ConstPow(..), root, sqrt, sq)