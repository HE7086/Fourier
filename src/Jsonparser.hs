{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase #-}

module Jsonparser where

import           Data.Aeson hiding (Series)
import qualified Data.Aeson.Types as T
import           Data.Attoparsec hiding (take)
import           Data.ByteString (pack)
import           Data.Complex
import           Data.Char (ord)

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
        _ -> undefined

getForm :: Series -> [Form]
getForm (Series xs) = map convert xs

jsonToCoord :: FilePath -> IO [(Int, Int)]
jsonToCoord path = do
    forms <- jsonToForm path
    return $ parseCoord forms

jsonToForm :: FilePath -> IO [Form]
jsonToForm path = do
    j <- readFile path
    let ans = case parse json $ pack $ map (fromIntegral . ord) j of
                Done _ v -> T.parseMaybe parseJSON v :: Maybe Series
                _        -> Nothing
      in case ans of
           Just an -> return $ getForm an
           Nothing -> return []

getCoords :: [FilePath] -> IO [(Int, Int)]
getCoords [] = return []
getCoords (p:path) = do
    cs <- jsonToCoord p
    rest <- getCoords path
    return $ cs ++ rest

