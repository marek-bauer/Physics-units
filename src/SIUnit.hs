{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances  #-}

module SIUnit
  ( SIUnit(..)
  , AddUnits
  , MulUnits
  , showUnits
  )
where

import TypeNumbers.Rational (QAdd, QMul, QSub, TRational, KnownRational(..))
import Data.Proxy (Proxy(..))
import Control.Monad (join)
import Data.Ratio (numerator, denominator)

data SIUnit :: * where
  Unit 
    :: TRational -- ^ second
    -> TRational -- ^ meter
    -> TRational -- ^ kilogram
    -> TRational -- ^ ampere
    -> TRational -- ^ kelvin
    -> TRational -- ^ mole
    -> TRational -- ^ candela
    -> SIUnit

type family AddUnits (x :: SIUnit) (y :: SIUnit) :: SIUnit where
  AddUnits (Unit s m kg a k mol cd) (Unit s' m' kg' a' k' mol' cd') 
    = Unit (QAdd s s') (QAdd m m') (QAdd kg kg') (QAdd a a') (QAdd k k') (QAdd mol mol') (QAdd cd cd')

type family MulUnits (x :: SIUnit) (c :: TRational) :: SIUnit where
  MulUnits (Unit s m kg a k mol cd) c
    = Unit (QMul s c) (QMul m c) (QMul kg c) (QMul a c) (QMul k c) (QMul mol c) (QMul cd c)

showUnits 
  :: forall s m kg a k mol cd p
  . (KnownRational s, KnownRational m, KnownRational kg, KnownRational a, KnownRational k, KnownRational mol, KnownRational cd) 
  => p (Unit s m kg a k mol cd)
  -> String
showUnits _ = join
  [ if seconds /= 0 then "s^" ++ printRat seconds else ""
  , if meters /= 0 then "m^" ++ printRat meters else ""
  , if kilograms /= 0 then "kg^" ++ printRat kilograms else ""
  , if amperes /= 0 then "a^" ++ printRat amperes else ""
  , if kelvins /= 0 then "K^" ++ printRat kelvins else ""
  , if mols /= 0 then "mol^" ++ printRat mols else ""
  , if candelas /= 0 then "cd^" ++ printRat candelas else ""
  ]
  where
    seconds = rationalVal (Proxy :: Proxy s)
    meters = rationalVal (Proxy :: Proxy m)
    kilograms = rationalVal (Proxy :: Proxy kg)
    amperes = rationalVal (Proxy :: Proxy a)
    kelvins = rationalVal (Proxy :: Proxy k)
    mols = rationalVal (Proxy :: Proxy mol)
    candelas = rationalVal (Proxy :: Proxy cd)

    printRat :: Rational -> String 
    printRat q 
      | den == 1 = show num
      | signum den == -1 = "-(" ++ show num ++ "/" ++ (show . abs $ den) ++ ")"
      | otherwise = "(" ++ show num ++ "/" ++ show den ++ ")"
      where
        num = numerator q
        den = denominator q

