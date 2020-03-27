{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Data.List (take)
import           Data.Char
import           Data.Complex
import           Data.Aeson hiding (Series)
import qualified Data.Aeson.Types as T
import           Data.Attoparsec hiding (take)
import           Data.ByteString (pack)
import           GHC.Generics
import           GHC.ST
import           Control.Arrow
import           Control.Monad
import           System.Environment
import           System.Directory

--
-- import           Codec.Picture.Drawing
-- import           Control.Monad.ST
-- import           Control.Monad.Primitive

---------- settings ----------
type Form = Double -> Complex Double

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

---------- 

main :: IO ()
main = do
    arg <- getArgs
    if length arg /= 3
       then error "usage: main <filetype> <sourcePath> <outputPath>"
       else do
        let srcPath = arg !! 1 ++ "/"
            outPath = arg !! 2 ++ "/"
        src <- map (srcPath ++)
            <$> (filterM (doesFileExist . (srcPath ++))
            =<< getDirectoryContents (arg !! 1))
        ans <- getCoords src
        createDirectoryIfMissing True outPath
        case head arg of
          "gif" -> do
              let name = outPath ++ filename ++ ".gif"
              case writeGifAnimation name 0 LoopingNever (generateImgStep ans) of
                  Left _ -> error "gif generation error"
                  Right _ -> putStrLn $ "Saved gif at: " ++ name
          "png" -> do
              let name = outPath ++ filename ++ ".png"
              savePngImage name $ ImageRGB8 $ generateImg ans
              putStrLn $ "Saved png at: " ++ name

          "evo" -> do
              let name i = outPath ++ show i ++ ".gif"
              fs <- mapM jsonToForm src
              mapM_ (\(f, i) ->
                    case writeGifAnimation (name i) 0 LoopingNever (generateImgAcc f) of
                        Left _ -> error "evo generation error"
                        Right _ -> putStrLn $ "Saved gif at: " ++ name i
                ) $ zip fs [1..]

          _ -> putStrLn "invalid parameters"


generateImgAcc :: [Form] -> [Image PixelRGB8]
generateImgAcc fs = gic 1 fs
    where gic i xs
            | i <= len = generateImg (parseCoord (take i xs)) : gic (i + 1) xs
            | otherwise = []
          len = length fs

generateImgStep :: [(Int, Int)] -> [Image PixelRGB8]
generateImgStep xs = gis 1 xs
    where gis i ys = if i < len
            then generateImg (take i ys) : gis (i + gifStep) ys
            else []
          len = length xs

generateImg :: [(Int, Int)] -> Image PixelRGB8
generateImg xs = runST $ do
    img <- createMutableImage width height white
    let w ys
            | null ys = freezeImage img
            | otherwise = do
                let (x,y) = head ys
                    pix = pixelArt x y
                writePixel img x y pix
                writePixel img x (y-1) pix
                writePixel img (x-1) y pix
                writePixel img (x-1) (y-1) pix
                writePixel img x (y+1) pix
                writePixel img (x+1) y pix
                writePixel img (x+1) (y+1) pix
                writePixel img (x+1) (y-1) pix
                writePixel img (x-1) (y+1) pix
                w $ tail ys
      in w xs

---------- compute ----------

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

---------- json parser ----------
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

---------- draw spinning vectors ----------

-- drawVector :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 -> Int -> Int -> Int -> Int -> m ()
-- drawVector pic i j x y = drawLine pic i j x y black

-- drawVectors :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 [Form] -> Double -> m ()
-- drawVectors pic (x:xs) t = do
