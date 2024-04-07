{-# OPTIONS_GHC -Wunused-top-binds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ((+), (-), (/), (*))
import BasicUnits
import GeneralizedOperations

main :: IO ()
main = putStrLn "Test suite not yet implemented"

newtonSecondLaw :: Fractional a => Force a -> Mass a -> Acceleration a
newtonSecondLaw force mass = force / mass

wavelength :: Fractional a => Speed a -> Frequency a -> Distance a
wavelength v f = v / f 

work :: Num a => Power a -> Time a -> Energy a 
work p t = p * t

-- distanceInAcceleratedMovement :: forall a. Fractional a => Acceleration a -> Time a -> Distance a 
-- distanceInAcceleratedMovement acc t = (acc * sq t) * (inv (2 :: a) :: a)