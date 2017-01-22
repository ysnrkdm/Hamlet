-- {-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
-- import Data.Word
import qualified EdaxProtocol

import System.IO
import System.Environment(getArgs, getProgName)
import System.Console.GetOpt
import Text.Printf

-- foreign export ccall search :: Word64 -> Word64 -> Int -> Int -> IO Int
--
-- search :: Word64 -> Word64 -> Int -> Int -> IO Int
-- search black white pnsLessThan searchDepth = do
--     putStrLn "Called!"
--     return $ 10 + 10

data HamletOptions = EvalOptions String | HashOptions String deriving Show

defaultOptions = EvalOptions ""

options :: [OptDescr HamletOptions]
options =
  [
    Option ['e'] ["eval"]
        (ReqArg EvalOptions "SFEN")
        "Eval given board and turn in SFEN",
    Option ['h'] ["hash"]
        (ReqArg HashOptions "SFEN")
        "Show hash of given board factors"
  ]

compilerOpts :: [String] -> String -> IO ([HamletOptions], [String])
compilerOpts argv progName =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = printf "Usage: %s [OPTION...]\n SFEN exmple: %s" progName
                        "\"----O-X------X-----XXXO-OXXXXXOO-XXOOXOOXXOXXXOO--OOOO-O----OO-- O\""

main :: IO ()
main = do
    argv <- getArgs
    progName <- getProgName
    (options, _) <- compilerOpts argv progName
    case length options of
        0 -> startEdaxLoop
        _ -> case head options of
            EvalOptions s -> printf "%s\n" $ show $ EdaxProtocol.evalHelper s
            HashOptions s -> printf "%s\n" $ show $ EdaxProtocol.hashHelper s
            _ -> startEdaxLoop

startEdaxLoop = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    EdaxProtocol.commandLoop (EdaxProtocol.Search EdaxProtocol.AlphaBeta) undefined
