module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           GHC.ST
import           Control.Monad
import           System.Environment
import           System.Directory
import           System.FilePath
import           Data.Char (toUpper)

import           Optparser
import           Jsonparser
import           Form
import           Settings

version :: String
version = "0.1.0.0"

main :: IO ()
main = do
    (opt,non) <- parseArgs =<< getArgs
    case opt of
      Options {optMode = Version} -> printVersion
      Options {optMode = Help} -> printHelp
      Options {optMode = Gif, optInput = input, optOutput = output, optCombined = combined}
        -> genGif input output combined
      Options {optMode = Png, optInput = input, optOutput = output, optCombined = combined}
        -> genPng input output combined

printVersion :: IO ()
printVersion = putStrLn $ "Simple Fun Fourier Drawing Program, version: " ++ version

printHelp :: IO ()
printHelp = do
    name <- getProgName
    putStrLn
        $ "usage: " ++ name ++ " <options>\n"
        ++ "available options:\n"
        ++ "-g --gif      set filetype as gif\n"
        ++ "-p --png      set filetype as png\n"
        ++ "-c --combined combine the images afterwards\n"
        ++ "-i --input    set input file/dirpath\n"
        ++ "-o --output   set output file/dirpath\n"
        ++ "-v --version  print program version\n"
        ++ "-h --help     print this help\n"

genGif :: FilePath -> FilePath -> Bool -> IO ()
genGif input output combined = do
    files <- parseFilePath input output
    outs <- mapM (`genGifInternal` output) files
    when combined $ combineGif outs

-- handle a file, assume is a valid json
genGifInternal :: FilePath -> FilePath -> IO FilePath
genGifInternal input output = do
    src <- jsonToForm input
    let out = getOutName input output ".gif"
    case writeGifAnimation out 0 LoopingForever (generateImgAcc src) of
        Left _ -> error $ "gif generation error by: " ++ out
        Right _ -> putStrLn ("Saved gif at: " ++ out) >> return out

genPng :: FilePath -> FilePath -> Bool -> IO ()
genPng input output combined = do
    files <- parseFilePath input output
    outs <- mapM (`genPngInternal` output) files
    when combined $ combinePng outs

genPngInternal :: FilePath -> FilePath -> IO FilePath
genPngInternal input output = do
    src <- jsonToCoord input
    let out = getOutName input output ".png"
    savePngImage out $ ImageRGB8 $ generateImg src
    putStrLn $ "Saved png at: " ++ out
    return out

parseFilePath :: FilePath -> FilePath -> IO [FilePath]
parseFilePath i o = do
    output <- makeAbsolute o
    unless (null $ takeExtensions output) $ error "invalid output path"
    createDirectoryIfMissing True output

    input <- makeAbsolute i
    case map toUpper $ takeExtensions input of
      ".json" -> return [input]
      "" -> filterM (return . isExtensionOf ".json") =<< getDirectoryContents input
      _ -> error "invalid input path"

    -- createDirectoryIfMissing True output
    -- isPath <- doesPathExist input
    -- isFile <- doesFileExist input
    -- isDirectory <- doesDirectoryExist input
    -- if not isPath
    --   then ioError (userError "filepath not exists, check your input")
    --   else if isFile
    --     then return [input]
    --     else if isDirectory
    --       then filterM doesFileExist =<< getDirectoryContents input
    --       else ioError (userError "unknown filepath, check your input")

-- test :: FilePath -> IO ()
-- test path = mapM_ ($ path)
--     [ doesDirectoryExist >=> flip when (putStrLn "is dir")
--     , doesFileExist >=> flip when (putStrLn "is file")
--     , flip unless (putStrLn "not exist") <=< doesPathExist
--     ]

generateImgAcc :: [Form] -> [Image PixelRGB8]
generateImgAcc fs = gic 1 fs
    where gic i xs
            | i <= len = generateImg (parseCoord (take i xs)) : gic (i + 1) xs
            | otherwise = []
          len = length fs

-- generateImgStep :: [(Int, Int)] -> [Image PixelRGB8]
-- generateImgStep xs = gis 1 xs
--     where gis i ys = if i < len
--             then generateImg (take i ys) : gis (i + gifStep) ys
--             else []
--           len = length xs

generateImg :: [(Int, Int)] -> Image PixelRGB8
generateImg xs = runST $ do
    img <- createMutableImage width height white
    let writePixels ys
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
                writePixels $ tail ys
      in writePixels xs

combineGif :: [FilePath] -> IO ()
combineGif = undefined

combinePng :: [FilePath] -> IO ()
combinePng = undefined

getOutName :: FilePath -> FilePath -> String -> FilePath
getOutName input output ext =
    replaceDirectory (replaceExtension input ext) (takeDirectory output)


---------- draw spinning vectors ----------

-- drawVector :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 -> Int -> Int -> Int -> Int -> m ()
-- drawVector pic i j x y = drawLine pic i j x y black

-- drawVectors :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 [Form] -> Double -> m ()
-- drawVectors pic (x:xs) t = do
