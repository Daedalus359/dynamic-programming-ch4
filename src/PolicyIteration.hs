module PolicyIteration where

import MDP
import OptTools (argMax, priorityOrFirst)

import Control.DeepSeq
import Data.Hashable
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Vector ((!))
import Data.Semigroup

--combine MDP with a discount factor to create an optimization problem with infinite discounted reward
data MDPTask s a = MDPTask (MDP s a) DiscountFactor
type DiscountFactor = Double

type Value = Double
type ValTable s = s -> Value

initZeros :: ValTable s
initZeros = const 0.0

vtFromVec :: (s -> Int) -> Vec.Vector Value -> ValTable s
vtFromVec indexF vals = (\s -> vals ! (indexF s))

type Policy s a = s -> a
--for this problem, assume deterministic policies only

polFromVec :: (s -> Int) -> Vec.Vector a -> Policy s a
polFromVec indexF actions s = actions ! (indexF s)

--this might ge better in the application specific code
initPolicy :: (Hashable s, Eq s) => MDP s a -> Policy s a
initPolicy (MDP (StateSpace states indexF) _ af) = polFromVec indexF actVec
  where
    actVec = fmap firstChoice states
    firstChoice s = case (af s) of
      [] -> error "action function returns no options"
      (a:as) -> a

q_pi :: Dynamics s a -> DiscountFactor -> ValTable s -> s -> a -> Value --computes q_pi(s, a) assuming valTable gives v_pi(s)
q_pi dynamics gamma vt state action = getSum $ foldMap f $ dynamics state $ action
  where
    f (s', r, p) = Sum $ (* p) $ (+ r) $ (* gamma) $ vt s'

policyEvaluation :: Double -> MDPTask s a -> Policy s a -> ValTable s -> ValTable s --returns the values under pi
policyEvaluation threshold (MDPTask (MDP (StateSpace states indexF) dynamics af) gamma) policy valTable = go valTable
  where
    go vt =
      if (delta < threshold)
        then newVT
        else go newVT
      where
        newVT = vtFromVec indexF refilledVec
        refilledVec = fmap (\s -> q_pi dynamics gamma vt s (policy s)) states
        delta = foldr max 0.0 $ fmap (\s -> abs $ (-) (newVT s) (vt s)) states

policyImprovement :: (Hashable s, Eq s, Eq a) => Double -> MDPTask s a -> Policy s a -> ValTable s -> (ValTable s, Policy s a)
policyImprovement threshold tsk@(MDPTask (MDP states dynamics af) gamma) policy valTable = go (False, valTable, policy)
  where
    go (stable, vt, pol) = if stable
      then (vt, pol)
      else go $ policyStep threshold tsk pol vt

--policyStep = undefined


policyStep :: (Hashable s, Eq s, Eq a) => Double -> MDPTask s a -> Policy s a -> ValTable s -> (Bool, ValTable s, Policy s a)
policyStep threshold tsk@(MDPTask (MDP (StateSpace states indexF) dynamics af) gamma) pol vt = (stable, evt, newPol)
  where
    evt = policyEvaluation threshold tsk pol vt

    newPol = polFromVec indexF $ fmap bestAction states
    bestAction s = fromMaybe (error "argmax failed at bestAction") $ argMax (priorityOrFirst $ pol s) (q_pi dynamics gamma evt s) $ af s

    stable = getAll $ foldMap (\s -> All $ (==) (newPol s) (pol s)) states
