module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           GHC.ST
import           Control.Monad
import           System.Environment
import           System.Directory
import           System.FilePath

import           Data.Char                      ( toLower )
import           Data.Either                    ( rights )
import           Data.List                      ( transpose )

import           OptParser
import           JsonParser
import           Form
import           Settings

main :: IO ()
main = do
    (opt, non) <- parseArgs =<< getArgs
    case opt of
        Options { optMode = Version } -> printVersion
        Options { optMode = Help }    -> printHelp
        Options { optMode = Gif, optInput = input, optOutput = output, optCombined = combined }
            -> genGif input output combined
        Options { optMode = Png, optInput = input, optOutput = output, optCombined = combined }
            -> genPng input output combined

printVersion :: IO ()
printVersion =
    putStrLn $ "Simple Fun Fourier Drawing Program, version: " ++ version

genGif :: FilePath -> FilePath -> Bool -> IO ()
genGif input output combined = do
    files <- parseFilePath input output
    outs  <- mapM (`genGifInternal` output) files
    when combined $ do
        frames <- combineGif outs
        saveGif (combine output "combined.gif") frames
        return ()

-- handle a file, assume is a valid json
genGifInternal :: FilePath -> FilePath -> IO FilePath
genGifInternal input output = do
    src <- jsonToForm input
    let out = getOutName input output ".gif"
    saveGif out (generateImgAcc src)

saveGif :: FilePath -> [Image PixelRGB8] -> IO FilePath
saveGif path imgs = case writeGifAnimation path 0 LoopingForever imgs of
    Left  msg -> error $ "gif generation error: " ++ msg
    Right gif -> gif >> putStrLn ("Saved gif at: " ++ path) >> return path
        --TODO: why is gif needed here to save the image?

genPng :: FilePath -> FilePath -> Bool -> IO ()
genPng input output combined = do
    files <- parseFilePath input output
    outs  <- mapM (`genPngInternal` output) files
    when combined $ do
        let outPath = combine output "combined.png"
        writePng outPath =<< combinePng outs
        putStrLn $ "Saved png at: " ++ outPath

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
    --TODO: use doesDirectoryExist instead?
    createDirectoryIfMissing True output

    input <- makeAbsolute i
    case map toLower $ takeExtensions input of
        ".json" -> return [input]
        ""      -> do
            files <- filterM (return . isExtensionOf ".json")
                =<< getDirectoryContents input
            return $ map (combine input) files
            --TODO: combine these two commands
        _ -> error $ "invalid input path: " ++ input

-- test :: FilePath -> IO ()
-- test path = mapM_ ($ path)
--     [ doesDirectoryExist >=> flip when (putStrLn "is dir")
--     , doesFileExist >=> flip when (putStrLn "is file")
--     , flip unless (putStrLn "not exist") <=< doesPathExist
--     ]

generateImgAcc :: [Form] -> [Image PixelRGB8]
generateImgAcc fs = gic 1 fs
  where
    gic i xs | i <= len  = generateImg (formsToCoords (take i xs)) : gic (i + 1) xs
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
    mapM_
        (\(x, y) -> do
            let pix = pixelArt x y
            --TODO: configurable thickness
            writePixel img x       y       pix
            writePixel img x       (y - 1) pix
            writePixel img (x - 1) y       pix
            writePixel img (x - 1) (y - 1) pix
            writePixel img x       (y + 1) pix
            writePixel img (x + 1) y       pix
            writePixel img (x + 1) (y + 1) pix
            writePixel img (x + 1) (y - 1) pix
            writePixel img (x - 1) (y + 1) pix
        )
        xs
    freezeImage img

combineGif :: [FilePath] -> IO [Image PixelRGB8]
combineGif files = do
    imgs' <- mapM readGifImages files
    let imgs = map (map convertRGB8) $ rights imgs'
    let len  = maximum $ map length imgs
    mapM combineImageRGB8 $ transpose $ map (makeSameLength len) imgs

-- in case any of the gif is too short, fill the rest part with the last frame
makeSameLength :: Int -> [Image PixelRGB8] -> [Image PixelRGB8]
makeSameLength len imgs = if length imgs /= len
    then imgs ++ replicate (len - length imgs) (last imgs)
    else imgs

combineImageRGB8 :: [Image PixelRGB8] -> IO (Image PixelRGB8)
combineImageRGB8 images = do
    img <- createMutableImage width height white
    mapM_
        (\image -> do
            let coords =
                    [ (x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1] ]
            mapM_
                (\(x, y) -> runST $ do
                    let pix = pixelAt image x y
                    if pix /= white
                        then return $ writePixel img x y pix
                        else mempty
                )
                coords
        )
        images
    freezeImage img

combinePng :: [FilePath] -> IO (Image PixelRGB8)
combinePng files = do
    imgs' <- mapM readPng files
    let imgs = map convertRGB8 $ rights imgs'
    combineImageRGB8 imgs

getOutName :: FilePath -> FilePath -> String -> FilePath
getOutName input output ext =
    replaceDirectory (replaceExtension input ext) output
