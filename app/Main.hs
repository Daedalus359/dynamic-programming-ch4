module Main where

import JacksRentals
import OptTools

import qualified Data.Set as Set

main :: IO ()
main = do
  print $ (argMaxByFirst jacksFinalVT eodStates, maximum $ fmap jacksFinalVT $ Set.toList eodStates)
  foldMap p $ reverse [0 .. maxCarsLot]
  print $ jacksFinalVT (maxCarsLot, maxCarsLot)

  print $ fmap jacksFinalPolicy $ ((,) maxCarsLot) <$> [0 .. maxCarsLot]

p i = print $ fmap jacksFinalPolicy $ ((,) i) <$> [0 .. maxCarsLot]

ex_4_1 :: IO Double
ex_4_1 = undefined
