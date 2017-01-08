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
import Control.Arrow

data Result = Result {va :: Eval.Value, pv :: [Move.Mv]} deriving (Eq, Ord)
instance Show Result where
    show (Result va pv) = "va: " ++ show va ++ ", pv : " ++ show pv

conv :: Move.Mv -> Result -> Result
conv mv res = Result (va res) (mv : pv res)

moves :: (BitBoard.Bb, Result) -> [(BitBoard.Bb, Result)]
moves (bd, result) =
    map (\ x -> (BitBoard.move bd x, conv x result)) $ MoveGenerator.moveGenerationFull bd

gametree :: BitBoard.Bb -> Tree.Tree (BitBoard.Bb, Result)
gametree p = Tree.reptree moves (p, Result 0 [])

-- alphabeta
alphabeta :: (Num a, Eq a) => a -> BitBoard.Bb -> Result
alphabeta depth = normalizeResult . minimum . Tree.minimize' . Tree.maptree ((Eval.eval . fst) &&& snd) . Tree.prune depth . gametree

alphabetaWEndSolver :: (Num a, Eq a) => a -> BitBoard.Bb -> Result
alphabetaWEndSolver depth bb
    | BitBoard.getNumVacant bb <= pnsDepth =
        if va pnsRes > 0 then
            SlackMessenger.unsafeSendMessageNow ("PNS Search success!: " ++ show pnsRes ++ ".") pnsRes else
            alphaRes
    | otherwise = alphaRes
    where
        pnsDepth = 10
        pnsRes = normalizeSolverResult (ProofNumberSearch.pnsSearch pnsDepth bb)
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