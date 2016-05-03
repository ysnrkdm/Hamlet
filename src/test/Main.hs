module Main where
-- friends
import qualified BitBoard
import qualified Piece
import qualified Search
import qualified ProofNumberSearch
import qualified Util
import qualified Move
-- GHC

-- libraries
import Text.Printf (printf)
import Criterion.Main

-- std
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.List

main :: IO ()
main = Test.Framework.defaultMain $ hUnitTestToTests $ TestList [
        "ProofNumberSearch Very Basic Test" ~: TestList [
            "pnsTest1" ~: pnsProven []          @=? pns 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-- O",
            "pnsTest2" ~: pnsProven []          @=? pns 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO-- O",
            "pnsTest3" ~: pnsProven ["Bh8"]     @=? pns 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOX- O",
            "pnsTest4" ~: pnsDisproven ["Bf8","Wg8"]  @=? pns 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX--- O",
            "pnsTest5" ~: pnsProven []          @=? pns 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX--- X",
            "pnsTest6" ~: pnsDisproven ["Bg8","Wh8"]  @=? pns 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX-- O",
            "pnsTest7" ~: pnsDisproven ["Bh8"]  @=? pns 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOX- O",
            "pnsTest8" ~: pnsDisproven ["Bh8"]  @=? pns 10 "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXOXXXXXXXOXXXXXOX- O",
            "pnsTest9" ~: pnsProven ["Bh8"]     @=? pns 10 "OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOXOOOOOOOXOOOOXXX- O"
        ],
        -- 12 depth, cannot execute right now (can freeze)
        "ProofNumberSearch Test - 12 depth" ~: TestList [
            "pnsTest01" ~: pnsProven ["Bb6"]        @=? pns 15 "O--XXXX-XXXXXXX-OXOXOOXXOOXXOXXXOOXXOXXXO-OOOOXX--OOOO-X--OOOO-- O",
            "pnsTest02" ~: pnsProven ["Ba3"]        @=? pns 15 "--XXXXX-OOXXXXX--OOXXXXOXXOOOXXOXXOOOXXOXXXXXOXOO-OOOOOO-----O-O O",
            "pnsTest03" ~: pnsProven ["Bg2"]        @=? pns 15 "-XXX----O-XXXX-OOXXXXXXXOXXOXXXXOXOXOOXXOOXXXXOXO-XXXOOO---XOO-O O",
            "pnsTest04" ~: pnsDisproven ["Bh1"]     @=? pns 15 "O--XOO--XOOXXXXXXXXOXXXXXXXXOXXX--XOOOX---OOXOOO-OOOOOOO-OOOOOO- O",
            "pnsTest05" ~: pnsProven ["Ba6"]        @=? pns 15 "OOOOOXO---OOOOO-XXOXXXXX-XXOOXXXXXXXXXXX-XOOOXXXX-OOOO----OOOOO- O",
            "pnsTest06" ~: pnsDisproven ["Ba4"]     @=? pns 15 "X-OOOOO-XXOOOOX-XOXOOXXX-XOXXOXX-OXOXXXXO-XOXXXO---OOXXO--XO-OXO O",
            "pnsTest07" ~: pnsDisproven ["Bh4"]     @=? pns 15 "-X-OOOO---XXXO---OOXOX-OOOOOXXX-OOOXOXXXOOXXXOXXOOOXXXXX--OXXXXO O",
            "pnsTest08" ~: pnsDisproven ["Be2"]     @=? pns 15 "XXXXXO--XOOX-O--XXXOOO--XXOOOOO-XOOXOXO-XOXXOOXOO-XXOXX--OOXXXXO O",
            "pnsTest09" ~: pnsDisproven ["Ba8"]     @=? pns 15 "--XXXXX-O-XXXX-XOXXXXXXXOXXXXXOXOXOXOOO-X-OOOOO--XOOOO---XXXXXXX O",
            "pnsTest10" ~: pnsProven ["Bc8"]        @=? pns 15 "O-OXXXXO-OXXXXX-XXOXXOX--XOOXOXOXXXOXXXOOXXXXXXO--XXXXX----OOOO- O",
            "pnsTest11" ~: pnsProven ["Ba8"]        @=? pns 15 "--OXXXXO--XOOOOOOXOXOXOOXXXOOOXOO-OXOXOO-OXOXOOO--XXOOO--XXXXO-- O",
            "pnsTest12" ~: pnsProven ["Ba8"]        @=? pns 15 "--XXXX----XXXX--OOOOOXOXOOOOOOXXOOOXOOXXOOXOXOX-OXXXXXXO---XXXXO O",
            "pnsTest13" ~: pnsDisproven ["Bh3"]     @=? pns 15 "-XXXXXX-OOOOXO--OOOXOXO-OXOOOOX-OXXXXXXXOOOOOOO-O-OXOO----OXOOOO O",
            "pnsTest14" ~: pnsDisproven ["Bb3"]     @=? pns 15 "-OOOOOO---XXXX--X-XXOXXXOOXXOXX-XOXXOXX-XOOXXXX-OOOOXOX--XXXXXXX O"
        ]
--         "AlphabeaSearch Test" ~: TestList [
--             "alphabetaTest" ~: alphabetaTest
--         ]
        ]

pns q r = res
    where
        res = ProofNumberSearch.proofNumberSearch t q board
        t = BitBoard.turn board
        board = BitBoard.fromString r
b = Piece.B
w = Piece.W

alphabetaTest = do
    print $ Search.alphabeta 8 BitBoard.initialBoard
    print $ Search.alphabeta 9 BitBoard.initialBoard
    1 @=? 1

pnsProven mvs = ProofNumberSearch.Result (ProofNumberSearch.ProofDisproofNumber 9223372036854775807 0) (map Move.fromString mvs)
pnsDisproven mvs = ProofNumberSearch.Result (ProofNumberSearch.ProofDisproofNumber 0 9223372036854775807) (map Move.fromString mvs)
pnsZeros mvs = ProofNumberSearch.Result (ProofNumberSearch.ProofDisproofNumber 0 0) (map Move.fromString mvs)
