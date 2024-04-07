{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeNumbers.Integer 
  ( TInt(..)
  , KnownInt(..)
  , INormalize
  , IAdd
  , ISub
  , INeg
  , Mul
  , IMul
  , INMul
  , IDiv
  , INDiv
  , IAbs
  )
where

import TypeNumbers.Natural ( Div, Nat, Mul, Sub, Add )
import GHC.TypeLits(KnownNat, natVal)
import Data.Type.Ord ( Compare )

data TInt :: * where
  Pos  :: Nat -> TInt
  Neg  :: Nat -> TInt

data P (i :: k) = P

class KnownInt (i :: TInt) where
  intVal :: proxy i -> Integer

instance forall n. (KnownNat n) => KnownInt (Pos n) where
  intVal :: forall proxy n. KnownNat n => proxy (Pos n) -> Integer
  intVal _ = natVal (P :: P n)

instance forall n. (KnownNat n) => KnownInt (Neg n) where
  intVal :: forall proxy n. KnownNat n => proxy (Neg n) -> Integer
  intVal _ = -(natVal (P :: P n))

type family INormalize (x :: TInt) :: TInt where
  INormalize (Neg 0) = Pos 0
  INormalize x       = x

type family IAdd (x :: TInt) (y :: TInt) :: TInt where
  IAdd (Pos x) (Pos y) = Pos (Add x y)
  IAdd (Neg x) (Pos y) = NSub' y x (Compare y x)
  IAdd (Pos x) (Neg y) = NSub' x y (Compare x y)
  IAdd (Neg x) (Neg y) = Neg (Add x y)

type family NSub' (x :: Nat) (y :: Nat) (o :: Ordering) :: TInt where
  NSub' x y LT = Neg (Sub y x)
  NSub' _ _ EQ = Pos 0
  NSub' x y GT = Pos (Sub x y)

type family INeg (x :: TInt) :: TInt where
  INeg (Pos x) = INormalize (Neg x)
  INeg (Neg x) = Pos x

type family ISub (x :: TInt) (y :: TInt) :: TInt where
  ISub x y = IAdd x (INeg y)

type family IMul (x :: TInt) (y :: TInt) :: TInt where
  IMul (Pos x') (Pos y') = Pos (Mul x' y')
  IMul (Neg x') (Neg y') = Pos (Mul x' y')
  IMul (Pos x') (Neg y') = INormalize (Neg (Mul x' y'))
  IMul (Neg x') (Pos y') = INormalize (Neg (Mul x' y'))

type family INMul (x :: TInt) (y :: Nat) :: TInt where
  INMul (Pos x') y = Pos (Mul x' y)
  INMul (Neg x') y = Neg (Mul x' y)

type family IDiv (x :: TInt) (y :: TInt) :: TInt where
  IDiv (Pos x') (Pos y') = Pos (Div x' y')
  IDiv (Neg x') (Neg y') = Pos (Div x' y')
  IDiv (Pos x') (Neg y') = INormalize (Neg (Div x' y'))
  IDiv (Neg x') (Pos y') = INormalize (Neg (Div x' y'))

type family INDiv (x :: TInt) (y :: Nat) :: TInt where
  INDiv (Pos x') y = Pos (Div x' y)
  INDiv (Neg x') y = Neg (Div x' y)

type family IAbs (x :: TInt) :: Nat where
  IAbs (Pos x') = x' 
  IAbs (Neg x') = x' 
