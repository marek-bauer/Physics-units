{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module SIValue
  (module SIValue)
where

import SIUnit ( SIUnit(..), AddUnits, SubUnits, MulUnits, showUnits )
import TypeNumbers.Rational ( QNeg', KnownRational(..), QPos', QPos, QZero )
import GHC.TypeLits( KnownNat, natVal )
import Data.Proxy ( Proxy(..) )
import Data.Kind ( Type )

newtype SIValue (u :: SIUnit) (a :: Type) = SIValue { value :: a }
  deriving (Eq, Ord)

type One = Unit QZero QZero QZero QZero QZero QZero QZero

lift :: a -> SIValue One a
lift = SIValue

class (ToSI x ~ SIValue u a) => AsSIValue x u a where
  asSIValue :: x -> ToSI x

type family ToSI (a :: Type) :: Type where
  ToSI (SIValue u a) = SIValue u a
  ToSI a = SIValue One a

instance {-# OVERLAPPABLE #-} (ToSI a ~ SIValue One a) => AsSIValue a One a where
  asSIValue = SIValue

instance {-# OVERLAPPING #-} AsSIValue (SIValue u a) u a where
  asSIValue = id

val :: (AsSIValue x u a) => x -> a 
val = value . asSIValue

infixl 6 +@
(+@) :: (Num a, AsSIValue x u a, AsSIValue y u a) => x -> y -> SIValue u a
x +@ y = SIValue $ val x + val y

infixl 6 -@
(-@) :: (Num a, AsSIValue x u a, AsSIValue y u a) => x -> y -> SIValue u a
x -@ y = SIValue $ val x - val y

infixl 7 *@
(*@) :: (Num a, AsSIValue x u a, AsSIValue y u' a) => x -> y -> SIValue (AddUnits u u') a
x *@ y = SIValue $ val x * val y

infixl 7 /@
(/@) :: (Fractional a, AsSIValue x u a, AsSIValue y u' a) => x -> y -> SIValue (SubUnits u u') a
x /@ y = SIValue $ val x / val y

infixr 8 ^@
(^@) :: (Num a, AsSIValue x u a, KnownNat n) => x -> p n -> SIValue (MulUnits u (QPos' n)) a
x ^@ p = SIValue $ (val x) ^ (natVal p)

infixr 8 **@
(**@) :: (Floating a, AsSIValue x u a, KnownRational q) => x -> p q -> SIValue (MulUnits u q) a
x **@ p = SIValue $ (val x) ** (fromRational . rationalVal $ p)

sq :: (Num a, AsSIValue x u a) => x -> SIValue (MulUnits u (QPos' 2)) a
sq x = SIValue $ val x * val x

root :: forall x a u n p. (Floating a, AsSIValue x u a, KnownNat n) => x -> p n -> SIValue (MulUnits u (QPos 1 n)) a
root x p = SIValue $ val x ** (1.0 / (fromRational . toRational . natVal $ p))

sqrt :: (Floating a, AsSIValue x u a) => x -> SIValue (MulUnits u (QPos 1 2)) a
sqrt x = SIValue . Prelude.sqrt $ val x 

instance (Show v, KnownRational s, KnownRational m, KnownRational kg, KnownRational a, KnownRational k, KnownRational mol, KnownRational cd) 
  => Show (SIValue (Unit s m kg a k mol cd) v) where
  show (SIValue x) 
    | null unitsStr = show x
    | otherwise = show x ++ " " ++ unitsStr
    where
      unitsStr = showUnits (Proxy :: Proxy (Unit s m kg a k mol cd))