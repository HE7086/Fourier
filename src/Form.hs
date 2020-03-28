module Form where

import Data.Complex
import Control.Arrow
import Settings

type Form = Double -> Complex Double

--f(t) = c * exp(n * i * 2pi * t)
form :: Complex Double -> Int -> Double -> Complex Double
form c n t = c * cis (fromIntegral n * 2 * pi * t)

-- exact coordinates
pixels :: [Form] -> [(Double, Double)]
pixels forms = map toCar $ ps timeIntervals forms
    where ps [] _ = []
          ps (t:ts) fs = sum (map ($t) fs) : ps ts fs

timeIntervals :: [Double]
timeIntervals = ti amount [1]
    where ti 0 ds = ds
          ti i ds = ti (i - 1) (interval * i : ds)
          amount = fromIntegral pointCount
          interval = 1 / (amount + 1)

parseCoord :: [Form] -> [(Int, Int)]
parseCoord fs = map (f *** f) $ pixels fs
    where f = (*) scaleFactor . round

toCar :: Complex Double -> (Double, Double)
toCar = realPart &&& imagPart
