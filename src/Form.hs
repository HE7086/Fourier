module Form where

import           Data.Complex
import           Control.Arrow
import           Settings
import           Types

--f(t) = c * exp(n * i * 2pi * t)
form :: Complex Double -> Int -> Double -> Complex Double
form c n t = c * cis (fromIntegral n * 2 * pi * t)

-- exact coordinates
formsToCoordsExact :: [Form] -> [(Double, Double)]
formsToCoordsExact forms = map (realPart &&& imagPart) $ ps timeIntervals forms
  where
    ps []       _  = []
    ps (t : ts) fs = sum (map ($t) fs) : ps ts fs

-- [0, 0.001, ...]
timeIntervals :: [Double]
timeIntervals = ti amount [1]
  where
    ti 0 ds = ds
    ti i ds = ti (i - 1) (interval * i : ds)
    amount   = fromIntegral pointCount
    interval = 1 / (amount + 1)

formsToCoords :: [Form] -> [(Int, Int)]
formsToCoords fs = map (f *** f) $ formsToCoordsExact fs
    where f = (*) scaleFactor . round
