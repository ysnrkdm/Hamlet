module Eval where

import qualified Piece
import qualified BitBoard
import qualified Data.ByteString as BS
import Data.Int
import Data.Word
import System.IO.Unsafe

-- should be removed later, moved to BitBoard.hs
import Data.Bits

type Value = Double
type Coef = Double

fv :: Int -> Value
fv i = (fromIntegral :: Int8 -> Value) . (fromIntegral :: Word8 -> Int8) $ BS.index fvbin i
-- fv i = [2,
--     99, 1.6, -5, 7.5, 6.1, 4.3, 4.8, 5,
--     99, 1.6, -5, 7.5, 6.1, 4.3, 4.8, 5,
--     99, 1.6, -5, 7.5, 6.1, 4.3, 4.8, 5,
--     99, 1.6, -5, 7.5, 6.1, 4.3, 4.8, 5,
--     20.0, 15, 3.2, 1.1
--     ] !! i

evalKind :: Value
evalKind = (fromIntegral :: Int8 -> Value) . (fromIntegral :: Word8 -> Int8) $ BS.index fvbin 0

showFv :: [[Value]]
showFv =
    case evalKind of
        0 -> [map fv [0..8]]
        1 -> [map fv [0..32]]
        2 ->
            [
                map fv [0],
                map fv [1..8],
                map fv [9..16],
                map fv [17..24],
                map fv [25..32],
                map fv [33..36]
            ]
        _ -> [[0.0]]

{-# NOINLINE fvbin #-}
fvbin :: BS.ByteString
fvbin = unsafePerformIO $ BS.readFile "/Users/Yoshinori/Documents/OneDrive/codes/Hamlet/data"
-- fvbin = unsafePerformIO $ BS.readFile "/home/ubuntu/projects/Hamlet/data"

eval :: BitBoard.Bb -> Value
eval board
    | BitBoard.isTerminal board = terminalValue board
    | otherwise = case evalKind of
        0 -> staticPositionEvalNoPhase board
        1 -> staticPositionEvalWPhase board
--         2 -> staticPositionPossibleMovesEvalWPhase board
        2 -> staticEval board
        _ -> 0.0

terminalValue :: BitBoard.Bb -> Value
terminalValue board@(BitBoard.Bb _ _ turn)
    | BitBoard.isTerminal board = fromIntegral $ BitBoard.getNumPiecesFor board turn - BitBoard.getNumPiecesFor board (BitBoard.oppositeSide turn)
    | otherwise = 0.0

staticPositionEvalNoPhase :: BitBoard.Bb -> Value
staticPositionEvalNoPhase board = position board 0 1

staticPositionEvalWPhase :: BitBoard.Bb -> Value
staticPositionEvalWPhase board = position board progress 16
    where progress = 60 - BitBoard.getNumVacant board

staticPositionPossibleMovesEvalWPhase :: BitBoard.Bb -> Value
staticPositionPossibleMovesEvalWPhase board = sum
    [
        position board progress 16,
        possibleMovesCoef progress * possibleMoves board
    ]
    where
        progress = 64 - BitBoard.getNumVacant board

staticEval :: BitBoard.Bb -> Value
staticEval board = sum
    [
        possibleMovesCoef progress * possibleMoves board,
        edgeCoef progress * edge board,
        fixedPiecesCoef progress * fixedPieces board,
        opennessCoef progress * openness board,
        position board progress 16
    ]
    where
        progress = 64 - BitBoard.getNumVacant board

position :: BitBoard.Bb -> Int -> Int -> Value
position board@(BitBoard.Bb _ _ colour) progress divisor = sum -- $ map fromIntegral
    [
        fv' 1 * boardMap 0x8100000000000081,      -- corner A
        fv' 2 * boardMap 0x4281000000008142,      -- corner neighbor H/V B
        fv' 3 * boardMap 0x0042000000004200,      -- corner neighbor diag C
        fv' 4 * boardMap 0x2400810000810024,      -- corner neighbors' neighbor D
        fv' 5 * boardMap 0x1800008181000018,      -- E
        fv' 6 * boardMap 0x003C424242423C00,      -- F
        fv' 7 * boardMap 0x0000240000240000,      -- G
        fv' 8 * boardMap 0x0000183C3C180000       -- H
    ]
    where
        boardMap bmap = fromIntegral $ BitBoard.getNumPiecesWithMaskFor board bmap colour
        p = coefIndex progress divisor
        fv' i = fv (i + p * 8)

coefIndex :: Int -> Int -> Int
coefIndex progress divisor = progress `div` divisor

opennessCoef :: Int -> Coef
opennessCoef progress = 1.0 * [2.5, 2.5, 3.5, 3.5] !! coefIndex progress 16

edgeCoef :: Int -> Coef
edgeCoef _ = 1.0

fixedPiecesCoef :: Int -> Coef
fixedPiecesCoef progress = 1.0 * [2.0, 2.0, 200.0, 200.0] !! coefIndex progress 16

possibleMovesCoef :: Int -> Coef
possibleMovesCoef progress = 1.0 * fv (1 + 32 + coefIndex progress 16)

openness :: BitBoard.Bb -> Value
openness board = opennessHelper board $ BitBoard.getBoardForTurn board

opennessHelper :: BitBoard.Bb -> BitBoard.BitBoard -> Value
opennessHelper _ 0 = 0
opennessHelper board bb =
    fromIntegral opennessAtHead + opennessHelper board newBb
    where
    opennessAtHead = BitBoard.numPeripherals board Piece.Empty pos
    (pos, newBb) = BitBoard.takeOneAndSetZero bb

-- Yet implemented but ok
edge :: BitBoard.Bb -> Value
edge _ = 0.0

fixedPieces :: BitBoard.Bb -> Value
fixedPieces = fixedPiecesHelper

fixedPiecesHelper :: BitBoard.Bb -> Value
fixedPiecesHelper board =
    fromIntegral $ sum list
    where
        list = map fixedPiecesEdgeHelper [rowTop, rowBottom, colLeft, colRight]
        bb = BitBoard.getBoardForTurn board
        rowTop = BitBoard.rowOf bb 1
        rowBottom = BitBoard.rowOf bb 8
        colLeft = BitBoard.colOf bb 1
        colRight = BitBoard.colOf bb 8

fixedPiecesEdgeHelper :: BitBoard.BitPattern8 -> Int
fixedPiecesEdgeHelper 0 = 0
fixedPiecesEdgeHelper 0xFF = 8
fixedPiecesEdgeHelper bb = (fixedPiecesEdgeHelperL bb) + (fixedPiecesEdgeHelperR bb)
    where
    fixedPiecesEdgeHelperL bb
        | popCount (bb .&. 0xFE) == 7 = 7
        | popCount (bb .&. 0xFC) == 6 = 6
        | popCount (bb .&. 0xF8) == 5 = 5
        | popCount (bb .&. 0xF0) == 4 = 4
        | popCount (bb .&. 0xE0) == 3 = 3
        | popCount (bb .&. 0xC0) == 2 = 2
        | popCount (bb .&. 0x80) == 1 = 1
        | otherwise = 0
    fixedPiecesEdgeHelperR bb
        | popCount (bb .&. 0x7F) == 7 = 7
        | popCount (bb .&. 0x3F) == 6 = 6
        | popCount (bb .&. 0x1F) == 5 = 5
        | popCount (bb .&. 0x0F) == 4 = 4
        | popCount (bb .&. 0x07) == 3 = 3
        | popCount (bb .&. 0x03) == 2 = 2
        | popCount (bb .&. 0x01) == 1 = 1
        | otherwise = 0

possibleMoves :: BitBoard.Bb -> Value
possibleMoves board = fromIntegral $ BitBoard.getNumPuttablesFor board (BitBoard.turn board)
