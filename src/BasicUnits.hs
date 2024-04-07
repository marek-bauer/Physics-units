{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module BasicUnits 
  ( module BasicUnits )
where

import TypeNumbers.Rational ( QNeg', QPos', QZero )
import SIUnit (SIUnit(..))
import SIValue (SIValue(..))

{- Time -}
type Second = Unit (QPos' 1) QZero QZero QZero QZero QZero QZero
type Time a = SIValue Second a

seconds :: a -> Time a
seconds = SIValue

{- Frequency -}
type Hertz = Unit (QNeg' 1) QZero QZero QZero QZero QZero QZero
type Frequency a = SIValue Hertz a

hertz :: a -> Frequency a
hertz = SIValue

{- Distance -}
type Meter = Unit QZero (QPos' 1) QZero QZero QZero QZero QZero
type Distance a = SIValue Meter a

meters :: a -> Distance a
meters = SIValue

{- Area -}
type SquareMeter = Unit QZero (QPos' 2) QZero QZero QZero QZero QZero
type Area a = SIValue SquareMeter a

squareMeters :: a -> Area a
squareMeters = SIValue

{- Volume -}
type CubicMeter = Unit QZero (QPos' 3) QZero QZero QZero QZero QZero
type Volume a = SIValue CubicMeter a

cubicMeters :: a -> Volume a
cubicMeters = SIValue

{- Speed -}
type MeterPerSecond = Unit (QNeg' 1) (QPos' 1) QZero QZero QZero QZero QZero
type Speed a = SIValue MeterPerSecond a

metersPerSecond :: a -> Speed a
metersPerSecond = SIValue

{- Acceleration -}
type MetersPerSecondSq = Unit (QNeg' 2) (QPos' 1) QZero QZero QZero QZero QZero
type Acceleration a = SIValue MetersPerSecondSq a

metersPerSecondSq :: a -> Acceleration a
metersPerSecondSq = SIValue

{- Mass -}
type Kilogram = Unit QZero QZero (QPos' 1) QZero QZero QZero QZero
type Mass a = SIValue Kilogram a

kilogram :: a -> Mass a
kilogram = SIValue

{- Force -}
type Newton = Unit (QNeg' 2) (QPos' 1) (QPos' 1) QZero QZero QZero QZero
type Force a = SIValue Newton a

newtons :: a -> Force a
newtons = SIValue

{- Energy -}
type Joule = Unit (QNeg' 2) (QPos' 2) (QPos' 1) QZero QZero QZero QZero
type Energy a = SIValue Joule a

joules :: a -> Energy a
joules = SIValue

kilowattHour :: Num a => a -> Energy a
kilowattHour = SIValue . (* 3600000)

{- Power -}
type Watt = Unit (QNeg' 3) (QPos' 2) (QPos' 1) QZero QZero QZero QZero
type Power a = SIValue Watt a

watts :: a -> Power a
watts = SIValue

{- Pressure -}
type Pascal = Unit (QNeg' 2) (QNeg' 1) (QPos' 1) QZero QZero QZero QZero
type Pressure a = SIValue Pascal a

pascals :: a -> Pressure a
pascals = SIValue

{- Current -}
type Ampere = Unit QZero QZero QZero (QPos' 1) QZero QZero QZero
type Current a = SIValue Ampere a

ampere :: a -> Current a
ampere = SIValue

{- Charge -}
type Coulomb = Unit (QPos' 1) QZero QZero (QPos' 1) QZero QZero QZero
type Charge a = SIValue Coulomb a

coulombs :: a -> Charge a
coulombs = SIValue

{- Voltage -}
type Volt = Unit (QNeg' 3) (QPos' 2) (QPos' 1) (QNeg' 1) QZero QZero QZero
type Voltage a = SIValue Volt a

volts :: a -> Voltage a
volts = SIValue

{- Resistance -}
type Ohm = Unit (QNeg' 3) (QPos' 2) (QPos' 1) (QNeg' 2) QZero QZero QZero
type Resistance a = SIValue Ohm a

ohms :: a -> Resistance a
ohms = SIValue

{- Conductance -}
type Siemens = Unit (QPos' 3) (QNeg' 2) (QNeg' 1) (QPos' 2) QZero QZero QZero
type Conductance a = SIValue Siemens a

siemens :: a -> Conductance a
siemens = SIValue

{- Capacitance -}
type Farad = Unit (QPos' 4) (QNeg' 2) (QNeg' 1) (QPos' 2) QZero QZero QZero
type Capacitance a = SIValue Farad a

farads :: a -> Capacitance a
farads = SIValue

{- Temperature -}
type Kelvin = Unit QZero QZero QZero QZero (QPos' 1) QZero QZero
type Temperature a = SIValue Kelvin a

kelvins :: a -> Temperature a
kelvins = SIValue

celsius :: (Fractional a) => a -> Temperature a
celsius = SIValue . (\x -> x - (27315 / 100))

fahrenheit :: (Fractional a) => a -> Temperature a
fahrenheit = celsius . (\x -> 5 * (x - 32) / 9)

{- Amount of substance -}
type Mol = Unit QZero QZero QZero QZero QZero (QPos' 1) QZero
type AmountOfSubstance a = SIValue Mol a

mols :: a -> AmountOfSubstance a
mols = SIValue

{- Luminous intensity -}
type Candela = Unit QZero QZero QZero QZero QZero QZero (QPos' 1)
type LuminousIntensity a = SIValue Candela a

candela :: a -> LuminousIntensity a
candela = SIValue
