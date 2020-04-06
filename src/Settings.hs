module Settings where

import Codec.Picture

{-# INLINE width #-}
width :: Int
width = 1024

{-# INLINE height #-}
height :: Int
height = 1024

{-# INLINE white #-}
white :: PixelRGB8
white = PixelRGB8 0xff 0xff 0xff

{-# INLINE black #-}
black :: PixelRGB8
black = PixelRGB8 0 0 0

{-# INLINE pixelArt #-}
pixelArt :: Int -> Int -> PixelRGB8
pixelArt = const . const black
-- pixelArt x y = PixelRGB8 (fromIntegral x) (fromIntegral y) (fromIntegral $ x + y)

{-# INLINE pointCount #-}
pointCount :: Int
pointCount = 9999

{-# INLINE gifStep #-}
gifStep :: Int
gifStep = pointCount `div` 100

{-# INLINE filename #-}
filename :: String
filename = "out"

{-# INLINE scaleFactor #-}
scaleFactor :: Int
scaleFactor = 1

