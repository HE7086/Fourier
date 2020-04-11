module Main where

import           Data.Complex

import           JsonParser
import           Types
import           Form

main :: IO ()
main = undefined

maxVectorCount :: Int
maxVectorCount = 100

nextNumber :: Int -> Int
nextNumber n = if n > 0 then negate n else negate n + 1

convert :: Points -> Series
convert ps = undefined

vectorOriginAproximate :: Points -> Series -> Bool
vectorOriginAproximate p s@(Series vs) =
    not (null vs) && (vectorAvgDistance p s < 1)

vectorAvgDistance :: Points -> Series -> Double
vectorAvgDistance (Points ps) s@(Series vs) =
    sum
            (map
                (\i -> do
                    let (Point x y) = ps !! i
                    let est         = sumVectors s (timeIntervals !! i)
                    sqrt
                        (  (realPart est - fromIntegral x)
                        ^^ 2
                        +  (imagPart est - fromIntegral y)
                        ^^ 2
                        )
                )
                [0 .. length ps - 1]
            )
        / fromIntegral (length ps)

sumVectors :: Series -> Double -> Complex Double
sumVectors (Series vs) t = sum $ map (\(Vec n r i) -> form (r :+ i) n t) vs

----------

getLinearAverage :: Double -> Double -> Double -> Int -> Int -> Double
getLinearAverage input x1 x2 y1 y2 = input * slope + intercept
  where
    slope     = fromIntegral (y2 - y1) / (x2 - x1)
    intercept = fromIntegral y2 - slope * x2

timeBetweenPoints :: Double -> PointWithTime -> PointWithTime -> Bool
timeBetweenPoints time (PointWithTime _ t1) (PointWithTime _ t2) = 
    floatCompare time t1 0.001 == 1 && floatCompare time t2 0.001 == -1

floatCompare :: Double -> Double -> Double -> Int
floatCompare x y tolerance = do
    let diff = abs $ x - y
    let mean = abs $ (x + y) / 2
    if isNaN (diff / mean) || diff / mean < tolerance
        then 0
        else if x > y then 1 else -1

