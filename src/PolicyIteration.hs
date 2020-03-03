module PolicyIteration where

import MDP

import Data.Map.Strict as Map

type Value = Double
type ValTable s = Map.Map s Value

initZeros :: StateSpace a -> ValTable a
initZeros = Map.fromSet (const 0.0)

