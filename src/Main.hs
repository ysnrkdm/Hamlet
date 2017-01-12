-- {-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
-- import Data.Word
import qualified EdaxProtocol

import System.IO
import System.Environment(getArgs, getProgName)
import System.Console.GetOpt

-- foreign export ccall search :: Word64 -> Word64 -> Int -> Int -> IO Int
--
-- search :: Word64 -> Word64 -> Int -> Int -> IO Int
-- search black white pnsLessThan searchDepth = do
--     putStrLn "Called!"
--     return $ 10 + 10

data Options = Options {optEval :: String} deriving Show

defaultOptions = Options {optEval = ""}

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['e'] ["eval"]
      (ReqArg (\g opts -> opts { optEval = g }) "\"----O-X------X-----XXXO-OXXXXXOO-XXOOXOOXXOXXXOO--OOOO-O----OO-- O\"")
      "Eval given board and turn in SFEN"
  ]

parseArgs :: IO Options
parseArgs = do
    argv <- getArgs
    progName <- getProgName
    case getOpt Permute options argv of
        (opts, [], []) -> return (foldl (flip id) defaultOptions opts)
        (_, _, errs) -> ioError (userError (concat errs ++ helpMessage))
            where
                header = "Usage: " ++ progName ++ " [OPTION...]"
                helpMessage = usageInfo header options

main :: IO ()
main = do
    options <- parseArgs
--     putStrLn $ show options
--     putStrLn $ show $ optEval options
    case optEval options of
        "" -> do
            hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
            EdaxProtocol.commandLoop (EdaxProtocol.Search EdaxProtocol.AlphaBeta) undefined
        _ -> do
            putStrLn $ show $ EdaxProtocol.evalHelper (optEval options)