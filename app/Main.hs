module Main where

import JacksRentals
import MDP
import OptTools
import PolicyIteration

import Data.Hashable
import qualified Data.Set as Set

main :: IO ()
main = do
  print $ (argMaxByFirst jacksFinalVT eodStates, maximum $ fmap jacksFinalVT $ Set.toList eodStates)
  foldMap p $ reverse [0 .. maxCarsLot]
  print $ jacksFinalVT (maxCarsLot, maxCarsLot)

  print $ fmap jacksFinalPolicy $ ((,) maxCarsLot) <$> [0 .. maxCarsLot]

p i = print $ fmap jacksFinalPolicy $ ((,) i) <$> [0 .. maxCarsLot]

printPolicy :: (EodState -> CarMoves) -> IO ()
printPolicy = undefined

polImprovIO :: (Hashable s, Eq s, Eq a) => Double -> MDPTask s a -> Policy s a -> ValTable s -> IO (ValTable s, Policy s a)
polImprovIO threshold tsk@(MDPTask (MDP states dynamics af) gamma) policy valTable = go (False, valTable, policy, 0)
  where
    go (stable, vt, pol, n) = (>>) (putStrLn ("Iteration " ++ (show n))) $ if (stable)
      then undefined
      else undefined

ex_4_1 :: IO Double
ex_4_1 = undefined
