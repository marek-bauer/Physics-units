{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances  #-}

module TypeNumbers.Rational 
  ( TRational
  , KnownRational(..)
  , QPos
  , QPos'
  , QZero
  , QNeg
  , QNeg'
  , QAdd
  , QSub
  , QNegate
  , QMul
  , QDiv
  , QInv
  )
where

import TypeNumbers.Integer
    ( IAbs, INDiv, INMul, IMul, INeg, IAdd, TInt(..), Mul, KnownInt(..) )
import GHC.TypeLits ( Div, Mod, Nat, KnownNat, natVal )
import Data.Type.Ord ( Compare )
import Data.Ratio ((%))

data TRational :: * where
  Q  :: TInt -> Nat -> TRational

data P (i :: k) = P

class KnownRational (i :: TRational) where
  rationalVal :: proxy i -> Rational

instance (KnownInt n, KnownNat d) => KnownRational (Q n d) where
  rationalVal :: forall proxy n d.(KnownInt n, KnownNat d) => proxy (Q n d) -> Rational
  rationalVal _ = intVal (P :: P n) % natVal (P :: P d)


type family Euclid (a :: Nat) (b :: Nat) :: Nat where
  Euclid a 0 = a
  Euclid a b = Euclid b (Mod a b) 

type family GCD (a :: Nat) (b :: Nat) :: Nat where
  GCD 0 b = b
  GCD a 0 = a
  GCD a b = GCD' a b (Compare a b)

type family GCD' (a :: Nat) (b :: Nat) (o :: Ordering) :: Nat where
  GCD' x y EQ = x
  GCD' x y LT = Euclid y x
  GCD' x y GT = Euclid x y

type QPos (x :: Nat) (y :: Nat) = QNormalize (Q (Pos x) y)
type QPos' (x :: Nat) = QNormalize (Q (Pos x) 1)
type QZero = Q (Pos 0) 1
type QNeg (x :: Nat) (y :: Nat) = QNormalize (Q (Neg x) y)
type QNeg' (x :: Nat) = QNormalize (Q (Neg x) 1)

type family QNormalize (a :: TRational) :: TRational where
  QNormalize (Q num den) = Q (INDiv num (GCD (IAbs num) den)) (Div den (GCD (IAbs num) den)) 

type family QAdd (x :: TRational) (y :: TRational) :: TRational where
  QAdd (Q n d) (Q n' d') = QNormalize (Q (IAdd (INMul n d') (INMul n' d)) (Mul d d'))

type family QNegate (x :: TRational) :: TRational where
  QNegate (Q n d) = Q (INeg n) d

type family QSub (x :: TRational) (y :: TRational) :: TRational where
  QSub x y = QAdd x (QNegate y)

type family QMul (x :: TRational) (y :: TRational) :: TRational where
  QMul (Q n d) (Q n' d') = QNormalize (Q (IMul n n') (Mul d d'))

type family QInv (x :: TRational) :: TRational where
  QInv (Q (Pos n) d) = Q (Pos d) n 
  QInv (Q (Neg n) d) = Q (Neg d) n 

type family QDiv (x :: TRational) (y :: TRational) :: TRational where
  QDiv x y = QMul x (QInv y)