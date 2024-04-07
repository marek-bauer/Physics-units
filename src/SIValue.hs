{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module SIValue
  ( SIValue(..), liftSIValue)
where

import Prelude hiding ( (+), (-), (*), (/), sqrt, const )
import SIUnit ( SIUnit(..), AddUnits, MulUnits, showUnits )
import GeneralizedOperations ( Add(..), Neg(..), Mul(..), Inv(..), ConstPow(..), ConstPower(..) )
import TypeNumbers.Rational ( QNeg', KnownRational(..), QPos', QZero )
import GHC.TypeLits( KnownNat, natVal )
import Data.Proxy ( Proxy(..) )
import Data.Kind ( Type )

newtype SIValue (u :: SIUnit) (a :: Type) = SIValue { value :: a }

type One = Unit QZero QZero QZero QZero QZero QZero QZero

liftSIValue :: a -> SIValue One a
liftSIValue = SIValue

instance {-# OVERLAPPING #-} Num a => Add (SIValue u a) (SIValue u a) (SIValue u a) where
  (SIValue x) + (SIValue y) = SIValue $ x + y

instance {-# OVERLAPPING #-} Num a => Add (SIValue One a) a (SIValue One a) where
  (SIValue x) + y = SIValue $ x + y

instance {-# OVERLAPPING #-} Num a => Add a (SIValue One a) (SIValue One a) where
  x + (SIValue y) = SIValue $ x + y

instance {-# OVERLAPPING #-} Num a => Neg (SIValue u a) (SIValue u a) where
  neg (SIValue x) = SIValue . neg $ x

instance {-# OVERLAPPING #-} (Num a, u'' ~ AddUnits u u') => Mul (SIValue u a) (SIValue u' a) (SIValue u'' a) where
  (SIValue x) * (SIValue y) = SIValue $ x * y

instance {-# OVERLAPPING #-} Num a => Mul (SIValue u a) a (SIValue u a) where
  (SIValue x) * y = SIValue $ x * y

instance {-# OVERLAPPING #-} Num a => Mul a (SIValue u a) (SIValue u a) where
  x * (SIValue y) = SIValue $ x * y

instance {-# OVERLAPPING #-} (Fractional a, MulUnits u (QNeg' 1) ~ u') => Inv (SIValue u a) (SIValue u' a) where
  inv (SIValue x) = SIValue . inv $ x

instance {-# OVERLAPPING #-} (Floating a, MulUnits u q ~ u', KnownRational q) => ConstPower (SIValue u a) q (SIValue u' a) where
  power p (SIValue x) = SIValue $ x ** fromRational (rationalVal p)

instance {-# OVERLAPPING #-} (Num a, MulUnits u (QPos' n) ~ u', KnownNat n) => ConstPow (SIValue u a) n (SIValue u' a) where
  pow p (SIValue x) = SIValue $ x ^ natVal p

instance (Show v, KnownRational s, KnownRational m, KnownRational kg, KnownRational a, KnownRational k, KnownRational mol, KnownRational cd) 
  => Show (SIValue (Unit s m kg a k mol cd) v) where
  show (SIValue x) 
    | null unitsStr = show x
    | otherwise = show x ++ " " ++ unitsStr
    where
      unitsStr = showUnits (Proxy :: Proxy (Unit s m kg a k mol cd))