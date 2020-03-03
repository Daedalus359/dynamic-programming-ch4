module OptTools where

argMax :: (Ord b, Foldable t) => (a -> a -> a) -> (a -> b) -> t a -> Maybe a
argMax prefFunc f t = snd $ foldr compareAndReplace (Nothing, Nothing) t
  where
    compareAndReplace a (Nothing, Nothing) = (Just $ f a, Just a)
    compareAndReplace a prev@(Just maxSoFar, Just argMSoFar) = let newVal = f a in let valComp = (compare newVal maxSoFar) in
      case valComp of
        GT -> (Just newVal, Just a)
        EQ -> (Just maxSoFar, Just $ prefFunc argMSoFar a)
        LT -> prev
    compareAndReplace _ _ = error "argMax: compareAndReplace pattern should not be a mix of Just and Nothing"

--sometimes useful to break ties in argmax by choosing the same value you chose in the past, leading to stability in iterative computations
priorityOrFirst :: Eq a => a -> a -> a -> a
priorityOrFirst preferred a1 a2 =
  if (a2 == preferred) then a2 else a1

--if you want to simply keep elements according to whichever you encountered first
argMaxByFirst :: (Ord b, Foldable t) => (a -> b) -> t a -> Maybe a
argMaxByFirst = argMax const --faster implementation?