{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module BasicUnits where

import TypeNumbers.Rational ( QNeg', QPos', QZero )
import SIUnit (SIUnit(..))
import SIValue (SIValue(..))

-- data SIUnit :: * where
--   Unit 
--     :: TRational -- ^ second
--     -> TRational -- ^ meter
--     -> TRational -- ^ kilogram
--     -> TRational -- ^ ampere
--     -> TRational -- ^ kelvin
--     -> TRational -- ^ mole
--     -> TRational -- ^ candela
--     -> SIUnit

type Second = Unit (QPos' 1) QZero QZero QZero QZero QZero QZero
type Time a = SIValue Second a

seconds :: Num a => a -> Time a
seconds = SIValue
  
type Meter = Unit QZero (QPos' 1) QZero QZero QZero QZero QZero
type Distance a = SIValue Meter a

meters :: Num a => a -> Distance a
meters = SIValue

type SquareMeter = Unit QZero (QPos' 2) QZero QZero QZero QZero QZero
type Area a = SIValue SquareMeter a

squareMeters :: Num a => a -> Area a
squareMeters = SIValue

type CubicMeter = Unit QZero (QPos' 3) QZero QZero QZero QZero QZero
type Volume a = SIValue CubicMeter a

cubicMeters :: Num a => a -> Volume a
cubicMeters = SIValue

type MeterPerSecond = Unit (QNeg' 1) (QPos' 1) QZero QZero QZero QZero QZero
type Speed a = SIValue MeterPerSecond a

metersPerSecond :: Num a => a -> Speed a
metersPerSecond = SIValue

type MetersPerSecondSq = Unit (QNeg' 2) (QPos' 1) QZero QZero QZero QZero QZero
type Acceleration a = SIValue MetersPerSecondSq a

metersPerSecondSq :: Num a => a -> Acceleration a
metersPerSecondSq = SIValue
