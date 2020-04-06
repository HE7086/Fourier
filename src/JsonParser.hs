{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase #-}

module JsonParser where

import           Data.Aeson              hiding ( Series )
import qualified Data.Aeson.Types              as T
import           Data.Attoparsec         hiding ( take )
import           Data.ByteString                ( pack )
import           Data.Complex
import           Data.Char                      ( ord )
import           Data.Maybe                     ( fromMaybe )

import           GHC.Generics

import           Form

data Vec = Vec {
    number :: Int,
    real   :: Double,
    imag   :: Double
} deriving (Show, Generic, ToJSON, FromJSON)

convert :: Vec -> Form
convert (Vec n r i) = form (r :+ i) n

newtype Series = Series [Vec]
    deriving (Show, Generic, ToJSON)

instance FromJSON Series where
    parseJSON = \case
        Object o -> (o .: "series") >>= fmap Series . parseJSON
        _        -> undefined

getForm :: Series -> [Form]
getForm (Series xs) = map convert xs

jsonToCoord :: FilePath -> IO [(Int, Int)]
jsonToCoord path = parseCoord <$> jsonToForm path

jsonToForm :: FilePath -> IO [Form]
jsonToForm path = do
    j <- readFile path
    case parse json $ pack $ map (fromIntegral . ord) j of
        Done _ v -> return $ getForm $ fromMaybe
            (Series [])
            (T.parseMaybe parseJSON v :: Maybe Series)
        _ -> return []

jsonToCoords :: [FilePath] -> IO [(Int, Int)]
jsonToCoords = foldr (mappend . jsonToCoord) $ return []
