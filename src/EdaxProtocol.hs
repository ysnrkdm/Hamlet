module EdaxProtocol (
    commandLoop,
    evalHelper,
    SearchingMethod(..),
    LearningMethod(..),
    Mode(..)
) where
-- friends
-- import qualified Util
-- import qualified Piece
import qualified BitBoard
import qualified Move
import qualified Search
import qualified Util
import qualified Eval
import qualified SlackMessenger

-- GHC

-- libraries

-- std
import System.Exit
-- import Control.Arrow
-- import Data.Array
-- import Data.Char
-- import Data.Maybe
-- import Data.Tuple
-- import Text.Regexq
import Text.Regex.Posix
import Text.Printf

-- Returns true if given string is in the form of acceptable 'move'
isMove :: String -> Bool
isMove str = (length str == 2) && (str =~ "^[a-hA-H][1-8]$" :: Bool)

isPass :: String -> Bool
isPass str = (length str == 2) && (str =~ "^[Pp][Ss]$" :: Bool)

data SearchingMethod = AlphaBeta | AlphaBetaWPNS
data LearningMethod = TDLambda
data Mode =
      Search { searchingmethod :: SearchingMethod }
    | Learn { learningmethod :: LearningMethod }

evalHelper sfenstring =
    Eval.eval bb
    where
     bb = BitBoard.fromString sfenstring

commandLoop mode bd = do
    sfens <- getLine
    let cmds = words sfens
    case head cmds of
        "init" -> putStrLn "" >> commandLoop mode BitBoard.initialBoard
        "quit" -> exitWith $ ExitFailure 1
        "undo" -> putStrLn "undo (Not yet implemented)"
        "redo" -> putStrLn "redo (Not yet implemented)"
        "verbose" ->
            case cmds !! 1 of
                "1" -> putStrLn (show bd) >> putStrLn "\n\n\n"
                "0" -> putStr ""
                "p" -> putStrLn $ show Eval.showFv
                "eval" -> printf "Eval: %d" (Eval.eval bd)
                _ -> putStr ""
        "go" -> case mode of
                (Search searchingmethod) -> do
                    let res@(Search.Result _ pv) = case searchingmethod of
                            AlphaBeta -> Search.alphabeta 7 bd
                            AlphaBetaWPNS -> Search.alphabetaWEndSolver 7 bd
                    case SlackMessenger.unsafeSendMessageNow (printf "%s" (show res)) head pv of
                        Move.Nil -> do
                            putStrLn "\n\n>Hamlet plays PS"
                            commandLoop mode $ BitBoard.move bd (head pv)
                        _ -> do
                            putStrLn $ "\n\n>Hamlet plays " ++ show (head pv)
                            commandLoop mode $ BitBoard.move bd (head pv)
                (Learn _) -> putStrLn "Not yet supported"
        "setboard" -> commandLoop mode $ BitBoard.fromString ((cmds !! 1) ++ " " ++ (cmds !! 2))
        "util" -> case cmds !! 1 of
                "bit" -> putStrLn (BitBoard.showBitBoard (read (cmds !! 2) :: BitBoard.BitBoard))
                "fv" -> putStrLn (show $ Eval.fv (read $ cmds !! 2 :: Int))
                "setmode" -> commandLoop (case cmds !! 2 of
                        "ab" -> Search AlphaBeta
                        "abwpns" -> Search AlphaBetaWPNS
                        _ -> Search AlphaBeta) bd
                _ -> putStr ""
        _   | isMove $ head cmds -> do
                putStrLn $ "\n\nYou play " ++ head cmds
                commandLoop mode $ BitBoard.moveByPos bd (Util.posFromUSI (head cmds))
            | isPass $ head cmds -> do
                putStrLn "\n\nYou play PS"
                commandLoop mode $ BitBoard.move bd Move.Nil
            | otherwise -> putStrLn ("undefined command.." ++ sfens)
    commandLoop mode bd -- next
