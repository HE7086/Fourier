module Optparser where

import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Exit
import           Control.Monad
import           Data.List

data Mode = Gif | Png | Help | Version

data Options = Options  { optMode      :: Mode
                        , optInput     :: String
                        , optOutput    :: String
                        , optCombined  :: Bool
                        }

defaultOptions :: Options
defaultOptions = Options { optMode     = Png
                         , optInput    = "./data"
                         , optOutput   = "./out"
                         , optCombined = False
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['g', 'G']
             ["gif", "GIF", "Gif"]
             (NoArg (\opt -> opt { optMode = Gif }))
             "Generate gif animation"
    , Option ['p', 'P']
             ["png", "PNG", "Png"]
             (NoArg (\opt -> opt { optMode = Png }))
             "Generate png image"
    , Option ['c', 'C']
             ["combined"]
             (NoArg (\opt -> opt { optCombined = False }))
             "Combine all the image afterwards"
    , Option ['i', 'I']
             ["in", "input"]
             (ReqArg (\arg opt -> opt { optInput = arg }) "FILE")
             "Input file"
    , Option ['o', 'O']
             ["out", "output"]
             (ReqArg (\arg opt -> opt { optOutput = arg }) "FILE")
             "Output file"
    , Option ['v', 'V']
             ["version"]
             (NoArg (\opt -> opt { optMode = Version }))
             "Print version"
    , Option ['h', 'H', '?']
             ["help"]
             (NoArg (\opt -> opt { optMode = Help }))
             "Show help"
    ]

parseArgs :: [String] -> IO (Options, [String])
parseArgs argv = case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, err) ->
        ioError (userError (concat err ++ usageInfo "Usage: " options))

-- parseArgs :: IO Options
-- parseArgs = do
--     args <- getArgs
--     progName <- getProgName
--     let header = "Usage: " ++ progName ++ " [OPTION...]"
--     let helpMessage = usageInfo header options
--     case getOpt RequireOrder options args of
--       (opts, [], []) ->
--         case foldM (flip id) defaultOptions opts of
--           Right opts -> return opts
--           Left errorMessage -> ioError (userError (errorMessage ++ "\n" ++ helpMessage))
--       (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))
