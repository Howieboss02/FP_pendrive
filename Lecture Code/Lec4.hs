

twice :: (t -> t) -> t -> t
twice f x = f (f x)

ex1 :: Integer
ex1 = twice (*2) 3
ex2 :: Integer
ex2 = twice (\x -> x*2) 3
ex3 :: Integer
ex3 = twice f 3
  where f x = x*2


many :: (t -> t) -> t -> t
many f = twice $ twice $ twice $ twice f


ex4 :: [Double]
ex4 = map (/ 2) [5..10]





applyFuns :: [a -> b] -> [a] -> [b]
applyFuns fs xs = 
  let fxs = zip fs xs
  in map (uncurry ($))   fxs
  -- map (\(f,x) -> f x) fxs

-- >>> applyFuns [(+1),(1-)] [4,5]
-- [5,-4]

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = filter (`elem` ys) xs

-- >>> intersect [1..20] [4,6..100]
-- [4,6,8,10,12,14,16,18,20]

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = 
  let pairWithYs x = map (\y -> (x,y)) ys
      allXYs = map pairWithYs xs
  in concat allXYs

-- >>> allPairs [1,2,3] [5,10]
-- [(1,5),(1,10),(2,5),(2,10),(3,5),(3,10)]



concat1 :: [a] -> [a]
concat1 = foldr (:) []

concat2 :: [[a]] -> [a]
concat2 = foldr (++) []

concat3 :: [[a]] -> [[a]]
concat3 = foldr (:) [[]]

concat4 :: [[[a]]] -> [[a]]
concat4 = foldr (++) [[]]


reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) [] 
