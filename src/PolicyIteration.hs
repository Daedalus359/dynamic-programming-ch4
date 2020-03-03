module PolicyIteration where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Monoid

type StateSpace s = Set.Set s
type Reward = Double
type Probability = Double

type Dynamics s a = s -> a -> [(s, Reward, Probability)]
--lawful dynamics functions should always return a list such that the probabilities sum to 0
testDynamics :: Dynamics s a -> s -> a -> Bool
testDynamics dyn s a = (== 1) $ getSum $ foldMap (\(s,r,p) -> Sum p) $ dyn s a

data MDP s a = MDP {states :: StateSpace s, dynamics :: Dynamics s a}

type Value = Double
type ValTable s = Map.Map s Value

initZeros :: StateSpace a -> ValTable a
initZeros = Map.fromSet (const 0.0)

