module EdaxProtocol (
    commandLoop,
    SearchingMethod(..),
    LearningMethod(..),
    Mode(..)
) where
-- friends
--import qualified Util
import qualified Piece
import qualified BitBoard
import qualified Move
import qualified Search
import qualified Util
import qualified Eval

-- GHC

-- libraries

-- std
import System.Exit
import Control.Arrow
--import Data.Array
--import Data.Char
--import Data.Maybe
--import Data.Tuple
--import Text.Regexq
import Text.Regex.Posix

-- Returns true if given string is in the form of acceptable 'move'
isMove :: String -> Bool
isMove str = (length str == 2) && (str =~ "^[a-hA-H][1-8]$" :: Bool)

isPass :: String -> Bool
isPass str = (length str == 2) && (str =~ "^[Pp][Ss]$" :: Bool)

data SearchingMethod = AlphaBeta
data LearningMethod = TDLambda
data Mode =
      Search { searchingmethod :: SearchingMethod }
    | Learn { learningmethod :: LearningMethod }

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
                _ -> putStr ""
        "go" -> case mode of
                (Search searchingmethod) -> do
                    let (Search.Result _ pv) = Search.alphabetaWEndSolver 7 bd
                    case head pv of
                        Move.Nil -> do
                            putStrLn "\n\n>Hamlet plays PS"
                            commandLoop mode $ BitBoard.move bd (head pv)
                        _ -> do
                            putStrLn $ "\n\n>Hamlet plays " ++ show (head pv)
                            commandLoop mode $ BitBoard.move bd (head pv)
                (Learn learningmethod) -> putStrLn "Not yet supported"
        _   | isMove $ head cmds -> do
                putStrLn $ "\n\nYou play " ++ head cmds
                commandLoop mode $ BitBoard.moveByPos bd (Util.posFromUSI (head cmds))
            | isPass $ head cmds -> do
                putStrLn "\n\nYou play PS"
                commandLoop mode $ BitBoard.move bd Move.Nil
            | otherwise -> putStrLn ("undefined command.." ++ sfens)
    commandLoop mode bd -- next
