{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}

module TypeNumbers.Natural 
  ( Nat
  , Add
  , Sub
  , Mul
  , Div
  , Mod
  , Pow
  )
where

import GHC.TypeLits ( type (+), type (*), type (-), type (^), Div, Mod, Nat )

type Add (x :: Nat) (y :: Nat) = x + y
type Sub (x :: Nat) (y :: Nat) = x - y
type Mul (x :: Nat) (y :: Nat) = x * y
type Pow (x :: Nat) (y :: Nat) = x ^ y