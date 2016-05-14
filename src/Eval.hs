module Eval (
    eval,
    showFv,
    Value
) where

import qualified Piece
import qualified BitBoard
import qualified Data.ByteString as BS
import Data.Int
import Data.Word
import System.IO.Unsafe
import System.Random
import Debug.Trace
import Text.Printf

type Value = Double
type Coef = Double

fv :: Int -> Value
fv i = (fromIntegral :: Int8 -> Value) . (fromIntegral :: Word8 -> Int8) $ BS.index fvbin i

evalKind :: Value
evalKind = (fromIntegral :: Int8 -> Value) . (fromIntegral :: Word8 -> Int8) $ BS.index fvbin 0

showFv :: [Value]
showFv =
    case evalKind of
        0 -> map fv [0..8]
        1 -> map fv [0..32]
        2 -> map fv [0..36]
        _ -> [0.0]

fvbin :: BS.ByteString
-- fvbin = unsafePerformIO $ BS.readFile "/Users/Yoshinori/Documents/OneDrive/codes/FlatReversi/subproc/data"
fvbin = unsafePerformIO $ BS.readFile "/home/ec2-user/projects/Hamlet/data"

randomUnsafeIO :: BitBoard.Bb -> Value
randomUnsafeIO board = fromIntegral $ (truncate ((fromIntegral (BitBoard.hashFromBitBoard board) / 1000000000.0) *
    (unsafePerformIO $ (randomRIO (0, 100) :: IO Double)))) `mod` 99907

eval :: BitBoard.Bb -> Value
eval board
    | BitBoard.isTerminal board = terminalValue board
    | otherwise = case evalKind of
        0 -> staticPositionEvalNoPhase board
        1 -> staticPositionEvalWPhase board
        2 -> staticPositionPossibleMovesEvalWPhase board
        _ -> 0.0

terminalValue :: BitBoard.Bb -> Value
terminalValue board@(BitBoard.Bb black white turn)
    | BitBoard.isTerminal board = fromIntegral $ BitBoard.getNumPiecesFor board turn - BitBoard.getNumPiecesFor board (BitBoard.oppositeSide turn)
    | otherwise = 0.0

staticPositionEvalNoPhase :: BitBoard.Bb -> Value
staticPositionEvalNoPhase board = if progress < 8 then randomUnsafeIO board else position board 0 1
    where progress = 60 - BitBoard.getNumVacant board

staticPositionEvalWPhase :: BitBoard.Bb -> Value
staticPositionEvalWPhase board = if progress < 8 then randomUnsafeIO board else position board progress 16
    where progress = 60 - BitBoard.getNumVacant board

staticPositionPossibleMovesEvalWPhase :: BitBoard.Bb -> Value
staticPositionPossibleMovesEvalWPhase board = if progress < 24 then randomUnsafeIO board else
    sum
    [
        position board progress 16,
        possibleMovesCoef progress * possibleMoves board
    ]
    where
        progress = 64 - BitBoard.getNumVacant board

-- staticEval :: BitBoard.Bb -> Value
-- staticEval board = if progress < 24 then randomUnsafeIO board else
--     sum
--     [
--         position board progress
--         positionCoef progress * position board,
--         opennessCoef progress * openness board,
--         numPiecesCoef progress * numPieces board,
--         possibleMovesCoef progress * possibleMoves board
--     ]
--     where
--         progress = 64 - BitBoard.getNumVacant board

position :: BitBoard.Bb -> Int -> Int -> Value
position board@(BitBoard.Bb _ _ colour) progress divisor = sum -- $ map fromIntegral
    [
        (fv' 1) * boardMap 0x8100000000000081,      -- corner A
        (fv' 2) * boardMap 0x4281000000008142,      -- corner neighbor H/V B
        (fv' 3) * boardMap 0x0042000000004200,      -- corner neighbor diag C
        (fv' 4) * boardMap 0x2400810000810024,      -- corner neighbors' neighbor D
        (fv' 5) * boardMap 0x1800008181000018,      -- E
        (fv' 6) * boardMap 0x003C424242423C00,      -- F
        (fv' 7) * boardMap 0x0000240000240000,      -- G
        (fv' 8) * boardMap 0x0000183C3C180000       -- H
    ]
    where
        boardMap bmap = fromIntegral $ BitBoard.getNumPiecesWithMaskFor board bmap colour
        p = progress `div` divisor
        fv' i = fv (i + p * 8)

-- positionCoef :: Int -> Coef
-- positionCoef progress = 1.0 * [2, 4, 6, 8, 8] !! (coefIndex progress 15)
--
coefIndex :: Int -> Int -> Int
coefIndex progress divisor = progress `div` divisor

-- opennessCoef :: Int -> Coef
-- opennessCoef progress = 1.0 * [8, 6, 6, 3, 3] !! (coefIndex progress 15)
--
-- numPiecesCoef :: Int -> Coef
-- numPiecesCoef progress = 1.0 * [0, 1, 50, 200, 9999] !! (coefIndex progress 15)
--
possibleMovesCoef :: Int -> Coef
possibleMovesCoef progress = 1.0 * fv (1 + 32 + (coefIndex progress 16))
--
-- openness :: BitBoard.Bb -> Value
-- openness board = opennessHelper board $ BitBoard.getBoardForTurn board
--
-- opennessHelper :: BitBoard.Bb -> BitBoard.BitBoard -> Value
-- opennessHelper board 0 = 0
-- opennessHelper board bb = (fromIntegral $ BitBoard.numPeripherals board Piece.Empty pos) + (opennessHelper board newBb)
--     where
--     (pos, newBb) = BitBoard.takeOneAndSetZero bb
--
-- numPieces :: BitBoard.Bb -> Value
-- numPieces board = fromIntegral $ BitBoard.getNumPiecesFor board (BitBoard.turn board)
--
possibleMoves :: BitBoard.Bb -> Value
possibleMoves board = fromIntegral $ BitBoard.getNumPuttablesFor board (BitBoard.turn board)