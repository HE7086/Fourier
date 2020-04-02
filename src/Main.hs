module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           GHC.ST
import           Control.Monad
import           System.Environment
import           System.Directory

import           Optparser
import           Jsonparser
import           Form
import           Settings

version :: String
version = "0.1.0.0"

main2 :: IO ()
main2 = do
    (opt,non) <- parseArgs =<< getArgs
    case opt of
      Options {optMode = Version} -> printVersion
      Options {optMode = Help} -> printHelp
      Options {optMode = Gif, optInput = input, optOutput = output} -> genGif input output
      Options {optMode = Png, optInput = input, optOutput = output} -> genPng input output

printVersion :: IO ()
printVersion = putStrLn $ "Simple Fun Fourier Drawing Program, version: " ++ version

printHelp :: IO ()
printHelp = do
    name <- getProgName
    putStrLn
        $ "usage: " ++ name ++ " <options>\n"
        ++ "available options:\n"
        ++ "-g --gif     set filetype as gif\n"
        ++ "-p --png     set filetype as png\n"
        ++ "-i --input   set input file/dirpath\n"
        ++ "-o --output  set output file/dirpath\n"
        ++ "-v --version print program version\n"
        ++ "-h --help    print this help\n"

genGif :: FilePath -> FilePath -> IO ()
genGif input output = undefined

genPng :: FilePath -> FilePath -> IO ()
genPng input output = undefined

parseFilePath :: FilePath -> FilePath -> IO [FilePath]
parseFilePath input output = do
    createDirectoryIfMissing True output
    isPath <- doesPathExist input
    isFile <- doesFileExist input
    isDirectory <- doesDirectoryExist input
    if not isPath 
    then ioError (userError "filepath not exists, check your input")
    else if isFile 
    then return [input]
    else if isDirectory
    then filterM doesFileExist =<< getDirectoryContents input
    else ioError (userError "unknown filepath, check your input")

-- test :: FilePath -> IO ()
-- test path = mapM_ ($ path)
--     [ doesDirectoryExist >=> flip when (putStrLn "is dir")
--     , doesFileExist >=> flip when (putStrLn "is file")
--     , flip unless (putStrLn "not exist") <=< doesPathExist
--     ]

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

combineGif :: [FilePath] -> IO (Image PixelRGB8)
combineGif = undefined

---------- draw spinning vectors ----------

-- drawVector :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 -> Int -> Int -> Int -> Int -> m ()
-- drawVector pic i j x y = drawLine pic i j x y black

-- drawVectors :: PrimMonad m => MutableImage (PrimState m) PixelRGB8
--                 [Form] -> Double -> m ()
-- drawVectors pic (x:xs) t = do
