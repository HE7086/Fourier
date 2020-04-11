{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Data.Aeson              hiding ( Series )
import           Data.Complex

type Form = Double -> Complex Double

data Vec = Vec {
    number :: Int,
    real   :: Double,
    imag   :: Double
} deriving (Show)

instance ToJSON Vec where
    toJSON (Vec number real imag) =
        object ["number" .= number, "real" .= real, "imag" .= imag]

instance FromJSON Vec where
    parseJSON = withObject "Vec"
        $ \v -> Vec <$> v .: "number" <*> v .: "real" <*> v .: "imag"

newtype Series = Series {
    series :: [Vec]
} deriving (Show)

instance ToJSON Series where
    toJSON (Series series) = object ["series" .= series]

instance FromJSON Series where
    parseJSON = withObject "Series" $ \s -> Series <$> s .: "series"

instance Semigroup Series where
    Series xs <> Series ys = Series $ xs <> ys

instance Monoid Series where
    mempty = Series []

data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show)

instance ToJSON Point where
    toJSON (Point x y) = object ["x" .= x, "y" .= y]

instance FromJSON Point where
    parseJSON = withObject "Point" $ \p -> Point <$> p .: "x" <*> p .: "y"

newtype Points = Points {
    points :: [Point]
} deriving (Show)

instance ToJSON Points where
    toJSON (Points ps) = object ["points" .= ps]

instance FromJSON Points where
    parseJSON = withObject "Points" $ \p -> Points <$> p .: "points"

instance Semigroup Points where
    Points xs <> Points ys = Points $ xs <> ys

instance Monoid Points where
    mempty = Points []

data PointWithTime = PointWithTime {
    p :: Point,
    t :: Double
}

addTimeInterval :: Points -> [PointWithTime]
addTimeInterval (Points ps) = zipWith PointWithTime ps timeIntervals
  where
    step          = 1 / fromIntegral (length ps)
    timeIntervals = ti [1]
    ti [] = undefined
    ti xs@(x : _) | x - step > 0 = (x - step) : xs
                  | otherwise    = xs


