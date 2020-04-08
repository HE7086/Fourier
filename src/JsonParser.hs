{-# LANGUAGE OverloadedStrings #-}

module JsonParser where

import           Data.Aeson              hiding ( Series )
import           Data.Aeson.Types               ( parseMaybe )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Complex                   ( Complex((:+)) )

import           Form

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
    (Series xs) <> (Series ys) = Series (xs <> ys)

instance Monoid Series where
    mempty = Series []

convert :: Vec -> Form
convert (Vec n r i) = form (r :+ i) n

getForm :: Series -> [Form]
getForm (Series xs) = map convert xs

jsonToCoord :: FilePath -> IO [(Int, Int)]
jsonToCoord path = formsToCoords <$> jsonToForm path

jsonToForm :: FilePath -> IO [Form]
jsonToForm path = do
    json <- BSL.readFile path
    case decode json of
        Just js -> return $ getForm js
        Nothing -> error $ "invalid json file: " ++ path

jsonToCoords :: [FilePath] -> IO [(Int, Int)]
jsonToCoords = foldr (mappend . jsonToCoord) mempty
