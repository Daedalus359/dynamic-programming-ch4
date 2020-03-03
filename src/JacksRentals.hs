module JacksRentals where

import MDP

type EodState = (Int, Int) --how many cars are in locations 1 and 2, respectively, at the end of the day?

type CarMoves = Int --how many cars moved from location 1 to location 2 (negative for other direction)

carMoveCost = 2

businessDynamics :: Dynamics EodState CarMoves --EodState -> CarMoves -> [(EodState, Reward, Probability)]
businessDynamics eodState carsMoved = undefined
  where
    transportCost = abs carsMoved * carMoveCost