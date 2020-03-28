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

import Jsonparser
import Form
import Settings
--
-- import           Codec.Picture.Drawing
-- import           Control.Monad.ST
-- import           Control.Monad.Primitive

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

---------- draw spinning vectors ----------

-- drawVector :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 -> Int -> Int -> Int -> Int -> m ()
-- drawVector pic i j x y = drawLine pic i j x y black

-- drawVectors :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 [Form] -> Double -> m ()
-- drawVectors pic (x:xs) t = do
