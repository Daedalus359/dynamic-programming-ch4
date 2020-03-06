module Main where

import JacksRentals
import MDP
import OptTools
import PolicyIteration

import Data.Hashable
import qualified Data.Set as Set

main :: IO ()
main = polImprovIO vtThreshold jacksTask jacksPolInit jacksVTInit >> return ()

{-  do
  print $ (argMaxByFirst jacksFinalVT eodStates, maximum $ fmap jacksFinalVT $ Set.toList eodStates)
  printPolicy jacksFinalPolicy
  print $ jacksFinalVT (maxCarsLot, maxCarsLot)

  print $ fmap jacksFinalPolicy $ ((,) maxCarsLot) <$> [0 .. maxCarsLot]
-}

printPolicy :: Policy EodState CarMoves -> IO ()
printPolicy pol = foldMap p $ reverse [0 .. maxCarsLot]
  where
    p i = print $ fmap pol $ ((,) i) <$> [0 .. maxCarsLot]

polImprovIO :: Double -> MDPTask EodState CarMoves -> Policy EodState CarMoves -> ValTable EodState -> IO (ValTable EodState, Policy EodState CarMoves)
polImprovIO threshold tsk@(MDPTask (MDP states dynamics af) gamma) policy valTable = go False valTable policy 0
  where
    go :: Bool -> ValTable EodState -> Policy EodState CarMoves -> Int -> IO (ValTable EodState, Policy EodState CarMoves)
    go stable vt pol n = (>>) (putStrLn ("Iteration " ++ (show n))) $ (>>) (printPolicy pol) $ if stable
      then return (vt, pol)
      else let (nextStable, nextVT, nextPol) = policyStep threshold tsk pol vt in go nextStable nextVT nextPol (n + 1)

ex_4_1 :: IO Double
ex_4_1 = undefined
