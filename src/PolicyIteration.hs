module PolicyIteration where

import MDP --(MDP, StateSpace, Reward, Probability, Dynamics)
import OptTools (argMax, priorityOrFirst)

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid

--combine MDP with a discount factor to create an optimization problem with infinite discounted reward
data MDPTask s a = MDPTask (MDP s a) DiscountFactor
type DiscountFactor = Double

type Value = Double
type ValTable s = s -> Value

initZeros :: (Ord s, Show s) => StateSpace s -> ValTable s
initZeros states = (\s -> fromMaybe (vtError $ "initZeros, state was: " ++ (show s) ) $ Map.lookup s $ Map.fromSet (const 0.0) states)

vtFromMap :: (Ord s) => Map.Map s Value -> ValTable s
vtFromMap map = (\s -> fromMaybe (vtError "vtFromMap") $ Map.lookup s map)

vtError :: String -> Value
vtError str = error $ "lookup in value table failed during function: " ++ str

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

q_pi :: Dynamics s a -> DiscountFactor -> ValTable s -> s -> a -> Value --computes q_pi(s, a) assuming valTable gives v_pi(s)
q_pi dynamics gamma vt state action = getSum $ foldMap f $ dynamics state $ action
  where
    f (s, r, p) = Sum $ (* p) $ (+ r) $ (* gamma) $ vt s

policyEvaluation :: (Ord s) => Double -> MDPTask s a -> Policy s a -> ValTable s -> ValTable s --returns the values under pi
policyEvaluation threshold (MDPTask (MDP states dynamics af) gamma) policy valTable = go valTable
  where
    go vt = let (delta, newVT) = vtFromMap <$> (foldr (accumValues vt) (0, Map.empty) states) in
    --go vtMap = let (delta, newVTMap) = (foldr (accumValues vtMap) (0, Map.empty) states) in
      if (delta < threshold)
        then newVT
        --then vtFromMap newVTMap
        else go newVT
        --else go newVTMap

    --for simplicity, this only ever uses the old value table when calculating new values, although better estimates could exist inside newVT (fix later)
    accumValues vt state (delta, newVT) = (newDelta, newMap)
      where
        newVal = q_pi dynamics gamma vt state $ policy state
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

    accumActions evt pol state (stableOld, polMapSoFar) = (stableNew, newPolMap)
      where
        oldAction = pol state
        newPolMap = Map.insert state newAction polMapSoFar--need to make sure tie-breaking favors old policy if applicable
        newAction = fromMaybe (error "argMax returned Nothing") $ argMax (priorityOrFirst oldAction) qp $ af state
        stableNew = stableOld && (oldAction == newAction)
        qp = q_pi dynamics gamma evt state