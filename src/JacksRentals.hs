module JacksRentals where

import MDP
import PolicyIteration

import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HMap

{-
Exercise 4.7 and Example 4.2 from Reinforcement Learning: an Introduction, 2 ed, by Sutton and Barto
-}

--PARAMETERS
rentalReward = 10
carMoveReward = negate 2

lambda1_rentals = 3 --how many rentals expected at location 1 per day (poisson parameter)
lambda2_rentals = 4

lambda1_returns = 3 --how many returns expected loc 1 (poisson param)
lambda2_returns = 2

jacksGamma = 0.9

maxCarsLot = 9 --how many cars may be in the lot at any time?

vtThreshold = 0.001 --affects accuracy vs runtime of value iteration
jpThreshold = (1 - poissonThreshold) ** 2 --0.0001--how high must the probability of an event be for the dynamics function to include it as a possbility
poissonThreshold = 0.999 --how high must the total probability mass be before poisson stops listing possibilities?

jacksMDP :: MDP EodState CarMoves
jacksMDP = MDP eodStates businessDynamics validCarsToMove

jacksTask = MDPTask jacksMDP jacksGamma

(jacksFinalVT, jacksFinalPolicy) = policyImprovement vtThreshold jacksTask jacksPolInit jacksVTInit

jacksVTInit = initZeros eodStates
jacksPolInit = polFromMap $ HMap.fromList $ fmap (\s -> (s, 0)) $ Set.toList eodStates
--jacksPolInit = initPolicy jacksMDP --does not initialize policy to always choose 0 as specified in problem, end result should be the same though

type EodState = (Int, Int) --how many cars are in locations 1 and 2, respectively, at the end of the day?
--luckily, (Int, Int) already has an instance of Ord

eodStates :: Set.Set EodState
eodStates = Set.fromList $ (,) <$> [0 .. maxCarsLot] <*> [0 .. maxCarsLot]

type CarMoves = Int --how many cars to move overnight from location 1 to location 2 (negative for other direction)

validCarsToMove :: EodState -> [CarMoves] --A(s) for this MDP
validCarsToMove (lot1Cs, lot2Cs) = [negate lot2Cs .. lot1Cs]

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
      | totalMass > 1 = error $ "poisson mass with lambda = " ++ (show lambda) ++ " sums to " ++ (show totalMass)
      | totalMass == 1 = Nothing
      | totalMass > poissonThreshold = Just $ ((n, 1 - totalMass), (n + 1, 1)) --important to make probability distribution sum to 1
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
    transportReward = fromIntegral $ abs carsMoved * carMoveReward --immediate reward (value is negative) of moving cars overnight --changes for the exercise variant of this problem
    morningState = overnightMove eodState carsMoved

type MorningState = (Int, Int)

overnightMove :: EodState -> CarMoves -> MorningState
overnightMove (l1c, l2c) carsTo2
  --cars return to national company before move occurs
  | l1c > maxCarsLot = overnightMove (maxCarsLot, l2c) carsTo2
  | l2c > maxCarsLot = overnightMove (l1c, maxCarsLot) carsTo2
  --impossible actions
  | carsTo2 > l1c = overnightMove (l1c, l2c) l1c
  | (negate carsTo2) > l2c = overnightMove (l1c, l2c) (negate l2c)
  --cars return to national company after move occurs
  | l2c + carsTo2 > maxCarsLot = overnightMove (l1c, l2c) (maxCarsLot - l2c)
  | l1c + (negate carsTo2) > maxCarsLot = overnightMove (l1c, l2c) (negate $ maxCarsLot - l1c)
  --1 normal case, catches the transformed forms of all errors
  | otherwise = (l1c - carsTo2, l2c + carsTo2)

rewardSalesResult :: MorningState 
                    -> (Int, Probability) 
                    -> (Int, Probability) 
                    -> (Int, Probability) 
                    -> (Int, Probability) 
                    -> Maybe (EodState, Reward, Probability) --not the true reward, that also depends on costs spent moving cars overnight
rewardSalesResult (carsM1, carsM2) (rentals1, p1) (rentals2, p2) (returns1, pp1) (returns2, pp2) =
  if jointProb > jpThreshold
    then Just (es, totalRentals * rentalReward, jointProb)
    else Nothing
  where
    es = (min maxCarsLot $ carsM1 + returns1 - rentals1, min maxCarsLot $ carsM2 + returns2 - rentals2)
    totalRentals = fromIntegral $ rentals1 + rentals2
    jointProb = p1 * p2 * pp1 * pp2

--starting from the number of cars present in the morning, calculates dynamics for the sales and returns over the course of the day
salesDynamics :: MorningState -> [(EodState, Reward, Probability)]
salesDynamics ms@(carsM1, carsM2) = catMaybes $ rewardSalesResult ms <$> rentalsL1 <*> rentalsL2 <*> returnsL1 <*> returnsL2
  where
    rentalsL1 = capRentals carsM1 $ poisson lambda1_rentals
    rentalsL2 = capRentals carsM2 $ poisson lambda2_rentals

    returnsL1 = poisson lambda1_returns
    returnsL2 = poisson lambda2_returns
