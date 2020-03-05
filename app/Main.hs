module Main where

import JacksRentals
import OptTools

import qualified Data.Set as Set

main :: IO ()
main = do
  print $ (argMaxByFirst jacksFinalVT eodStates, maximum $ fmap jacksFinalVT $ Set.toList eodStates)
  print $ fmap jacksFinalPolicy $ ((,) 0) <$> [0 .. maxCarsLot]
  print $ jacksFinalVT (maxCarsLot, maxCarsLot)

ex_4_1 :: IO Double
ex_4_1 = undefined