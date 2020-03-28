module Settings where

import Codec.Picture

width :: Int
width = 1024

height :: Int
height = 1024

white :: PixelRGB8
white = PixelRGB8 0xff 0xff 0xff

black :: PixelRGB8
black = PixelRGB8 0 0 0

pixelArt :: Int -> Int -> PixelRGB8
pixelArt = const . const black
-- pixelArt x y = PixelRGB8 (fromIntegral x) (fromIntegral y) (fromIntegral $ x + y)

pointCount :: Int
pointCount = 9999

gifStep :: Int
gifStep = pointCount `div` 100

filename :: String
filename = "out"

scaleFactor :: Int
scaleFactor = 1

