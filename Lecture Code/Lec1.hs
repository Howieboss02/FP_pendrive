
module Lec1 where

import Prelude hiding (concat)

main :: IO ()
main = putStrLn "Functional programming is FUN"



cylinderSurfaceArea :: Double -> Double -> Double
cylinderSurfaceArea r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^2
  in  sideArea + 2 * topArea





doubleSmallNumber :: Int -> Int
doubleSmallNumber x =
    if isSmall x then 2*x else x
  where
    isSmall x = x < 10





addMod3Is2 :: [Int] -> [Int]
addMod3Is2 = undefined

prop_addMod3Is2_example :: Bool
prop_addMod3Is2_example = 
  addMod3Is2 [2, 3, 4, 8] == [5, 11]





concat :: [[Int]] -> [Int]
concat = undefined

prop_concat_example :: Bool
prop_concat_example =
  concat [[1],[2,3],[]] == [1,2,3]




qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
  where
    ys = [ y | y <- xs , y <= x ] 
    zs = [ z | z <- xs , z >  x ]
