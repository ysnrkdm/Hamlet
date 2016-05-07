module Search (
    alphabeta,
    alphabetaWEndSolver,
    Result(..)
) where

-- friends
import qualified BitBoard
import qualified Move
import qualified MoveGenerator
import qualified Eval
import qualified Tree
import qualified ProofNumberSearch
import qualified SlackMessenger
-- GHC

-- libraries

-- std
import Data.Function
import Data.List
import Data.Ord
import Debug.Trace
import Control.Arrow

data Result = Result {va :: Eval.Value, pv :: [Move.Mv]} deriving (Eq, Ord)
instance Show Result where
    show (Result va pv) = "va: " ++ show va ++ ", pv : " ++ show pv

conv :: Move.Mv -> Result -> Result
conv mv res = Result (-va res) (mv : pv res)

--minmax :: Int -> BitBoard.Bb -> Result
--minmax 0 bd = Result (Eval.eval bd) []
--minmax dep bd = maximumBy (compare `on` va) nexts
--    where
--        nexts = map next $ MoveGenerator.moveGenerationFull bd
--        next mv = conv mv . minmax (dep - 1) $ BitBoard.move bd mv

moves :: (BitBoard.Bb, Result) -> [(BitBoard.Bb, Result)]
moves (bd, result) =
    map (\ x -> (BitBoard.move bd x, conv x result)) $ MoveGenerator.moveGenerationFull bd

gametree :: BitBoard.Bb -> Tree.Tree (BitBoard.Bb, Result)
gametree p = Tree.reptree moves (p, Result 0 [])

--evaluate :: BitBoard.Bb -> Eval.Value
--evaluate = Tree.maximize . Tree.maptree (Eval.eval . fst) . (Tree.prune 3) . gametree

-- alphabeta
alphabeta :: (Num a, Eq a) => a -> BitBoard.Bb -> Result
-- alphabeta depth = normalizeResult . maximum . Tree.maximize' . Tree.maptree (\x -> ((Eval.eval . fst) x, snd x)) . (Tree.prune depth) . gametree
alphabeta depth = normalizeResult . maximum . Tree.maximize' . Tree.maptree ((Eval.eval . fst) &&& snd) . Tree.prune depth . gametree

alphabetaWEndSolver :: (Num a, Eq a) => a -> BitBoard.Bb -> Result
alphabetaWEndSolver depth bb
    | BitBoard.getNumVacant bb <= 10 = if va pnsRes > 0 then SlackMessenger.unsafeSendMessageNow ("PNS Search: point" ++ show pnsRes ++ ".") pnsRes else alphaRes
    | otherwise = alphaRes
    where
        pnsRes = normalizeSolverResult (ProofNumberSearch.pnsSearch 10 bb)
        alphaRes = alphabeta depth bb

normalizeSolverResult :: ProofNumberSearch.Result -> Result
normalizeSolverResult pnsRes = Result (normalizePNSValue $ ProofNumberSearch.va pnsRes) (ProofNumberSearch.pv pnsRes)

normalizePNSValue :: ProofNumberSearch.ProofDisproofNumber -> Eval.Value
normalizePNSValue val = case val of
    ProofNumberSearch.LeafProven -> 999
    ProofNumberSearch.LeafDisproven -> -999
    ProofNumberSearch.LeafUnknown -> 0
    ProofNumberSearch.ProofDisproofNumber {ProofNumberSearch.proof = n, ProofNumberSearch.disproof = m} ->
        if n > m then 9999 else -9999

normalizeResult :: (Eval.Value, Result) -> Result
normalizeResult (value, result) =
    Result value pvs
    where
        pvs = if not (null pvFromRes) then pvFromRes else [Move.Nil]
        pvFromRes = reverse $ pv result