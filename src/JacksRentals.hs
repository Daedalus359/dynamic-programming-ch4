module JacksRentals where

import MDP

import Data.List (unfoldr)
import qualified Data.Set as Set

{-
Exercise 4.7 and Example 4.2 from Reinforcement Learning: an Introduction, 2 ed, by Sutton and Barto
-}

jacksMDP :: MDP EodState CarMoves
jacksMDP = MDP eodStates businessDynamics validCarsToMove

type EodState = (Int, Int) --how many cars are in locations 1 and 2, respectively, at the end of the day?
--luckily, (Int, Int) already has an instance of Ord

eodStates :: Set.Set EodState
eodStates = Set.fromList $ (,) <$> [0 .. 20] <*> [0 .. 20]

type CarMoves = Int --how many cars to move overnight from location 1 to location 2 (negative for other direction)

validCarsToMove :: EodState -> [CarMoves] --A(s) for this MDP
validCarsToMove (lot1Cs, lot2Cs) = [negate lot2Cs .. lot1Cs]

carMoveReward = negate 2

lambda1_rentals = 3 --how many rentals expected at location 1 per day (poisson parameter)
lambda2_rentals = 4

lambda1_returns = 3 --how many returns expected loc 1 (poisson param)
lambda2_returns = 2

poissonThreshold = 0.9999 --how high must the total probability mass be before poisson stops listing possibilities?

factorial :: Int -> Int
factorial 0 = 1
factorial n
  | n > 0 = factorial (n - 1) * n
  | n < 0 = error "factorial not defined for n < 0"

eul_const :: Double
eul_const = 2.71828

poisson :: Int -> [(Int, Probability)]
poisson lambda = unfoldr f (0, 0) 
  where
    f (n, totalMass)
      | totalMass > poissonThreshold = Nothing
      | otherwise = Just $ ((n, pMass), (n + 1, totalMass + pMass))
        where pMass = poissonMass lambda n

--if there are only x cars, then all poisson probabilities for values >= x get lumped together under x
capRentals :: Int -> [(Int, Probability)] -> [(Int, Probability)]
capRentals limit [] = [] --this case will occur if the limit is higher than what can realistically be sold in a day (depends on lambda, may not actually appear)
capRentals limit lst@(entry@(numSales, p) : rest)
  | numSales < limit = (:) entry $ capRentals limit rest
  | otherwise = [(numSales, sumProb)]
    where sumProb = sum $ snd <$> lst

poissonMass :: Int -> Int -> Probability --PMF for poisson
poissonMass lambda n = (fromIntegral $ lambda ^ n) * (eul_const ** (fromIntegral $ negate lambda)) / (fromIntegral $ factorial n)

businessDynamics :: Dynamics EodState CarMoves --EodState -> CarMoves -> [(EodState, Reward, Probability)]
businessDynamics eodState carsMoved = fmap (\(s, r, p) -> (s, r + transportReward, p)) $ salesDynamics morningState
  where
    transportReward = fromIntegral $ abs carsMoved * carMoveReward --immediate reward (value is negative) of moving cars overnight
    morningState = overnightMove eodState carsMoved

type MorningState = (Int, Int)

overnightMove :: EodState -> CarMoves -> MorningState
overnightMove (l1c, l2c) carsTo2
  --4 cases involve cars returning to national company or impossible actions
  | l1c > 20 = overnightMove (20, l2c) carsTo2
  | l2c > 20 = overnightMove (l1c, 20) carsTo2
  | carsTo2 > l1c = overnightMove (l1c, l2c) l1c
  | (negate carsTo2) > l2c = overnightMove (l1c, l2c) (negate l2c)
  --1 normal case, catches the transformed forms of all errors
  | otherwise = (l1c - carsTo2, l2c + carsTo2)

--starting from the number of cars present in the morning, calculates dynamics for the sales and returns over the course of the day
salesDynamics :: MorningState -> [(EodState, Reward, Probability)]
salesDynamics (carsM1, carsM2) = undefined
  where
    rentalsL1 = capRentals carsM1 $ poisson lambda1_rentals
    rentalsL2 = capRentals carsM2 $ poisson lambda2_rentals