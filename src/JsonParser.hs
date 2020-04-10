{-# LANGUAGE OverloadedStrings #-}

module JsonParser where

import           Data.Aeson              hiding ( Series )
import           Data.Aeson.Types               ( parseMaybe )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Complex                   ( Complex((:+)) )

import           Form
import           Types

{- json parser for drawer -}

vecToForm :: Vec -> Form
vecToForm (Vec n r i) = form (r :+ i) n

seriesToForm :: Series -> [Form]
seriesToForm (Series xs) = map vecToForm xs

jsonToCoord :: FilePath -> IO [(Int, Int)]
jsonToCoord path = formsToCoords <$> jsonToForm path

jsonToForm :: FilePath -> IO [Form]
jsonToForm path = do
    json <- BSL.readFile path
    case decode json of
        Just js -> return $ seriesToForm js
        Nothing -> error $ "invalid json file: " ++ path

jsonToCoords :: [FilePath] -> IO [(Int, Int)]
jsonToCoords = foldr (mappend . jsonToCoord) mempty

{- json parser for converter -}

