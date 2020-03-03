module PolicyIteration where

import MDP --(MDP, StateSpace, Reward, Probability, Dynamics)

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid

--combine MDP with a discount factor to create an optimization problem with infinite discounted reward
data MDPTask s a = MDPTask (MDP s a) DiscountFactor
type DiscountFactor = Double

type Value = Double
type ValTable s = s -> Value

initZeros :: (Ord s) => StateSpace s -> ValTable s
initZeros states = (\s -> fromMaybe vtError $ Map.lookup s $ Map.fromSet (const 0.0) states)

vtFromMap :: (Ord s) => Map.Map s Value -> ValTable s
vtFromMap map = (\s -> fromMaybe vtError $ Map.lookup s map)

vtError :: Value
vtError = error "lookup in value table failed"

type Policy s a = s -> a
--for this problem, assume deterministic policies only
--implementations of policy can use Data.Map and fromMaybe with error

--this might ge better in the application specific code
initPolicy :: (Ord s) => MDP s a -> Policy s a
initPolicy (MDP states _ af) = (\s -> fromMaybe polError $ Map.lookup s $ Map.fromSet firstChoice states)
  where
    firstChoice s = case (af s) of
      [] -> error "action function returns no options"
      (a:as) -> a

polFromMap :: (Ord s) => Map.Map s a -> Policy s a
polFromMap map = (\s -> fromMaybe polError $ Map.lookup s map)

polError = error "lookup in policy map failed"

type PolValueUpdate s a = Dynamics s a -> DiscountFactor -> Policy s a -> ValTable s -> s -> Value--for use in step 2 of policy iteration

policyEvaluation :: (Ord s) => Double -> MDPTask s a -> Policy s a -> ValTable s -> ValTable s --returns the values under pi
policyEvaluation threshold (MDPTask (MDP states dynamics af) gamma) policy valTable = go valTable
  where
    go vt = let (delta, newVT) = vtFromMap <$> (foldr (accumValues vt) (0, Map.empty) states) in
      if (delta < threshold)
        then newVT
        else go newVT

    --for simplicity, this only ever uses the old value table when calculating new values, although better estimates could exist inside newVT (fix later)
    accumValues vt state (delta, newVT) = (newDelta, newMap)
      where
        newVal = getSum $ foldMap f $ dynamics state $ policy state
        f (s, r, p) = Sum $ (* p) $ (+ r) $ (* gamma) $ vt s
        oldVal = vt state
        newDelta = max delta $ abs $ newVal - oldVal
        newMap = Map.insert state newVal newVT

policyImprovement :: (Ord s, Eq a) => Double -> MDPTask s a -> Policy s a -> ValTable s -> (ValTable s, Policy s a)
policyImprovement threshold tsk@(MDPTask (MDP states dynamics af) gamma) policy valTable = go policy valTable
  where
    go pol vt = let evt = (evaluatedVT pol vt) in let (stable, newPol) = polFromMap <$> (foldr (accumActions evt pol) (True, Map.empty) states) in
      if stable
        then (evt, newPol)
        else go newPol evt

    evaluatedVT = policyEvaluation threshold tsk

    accumActions evt pol state (stableOld, polSoFar) = (stableNew, newPol)
      where
        newPol = undefined --need to make sure tie-breaking favors old policy if applicable
        stableNew = stableOld && ((pol state) == (newPol state))