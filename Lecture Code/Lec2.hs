
{-# OPTIONS_GHC -XTypeFamilies #-}

module Lec2 where

import Prelude hiding (concat, (&&))







-- * Only keep values that are equal to 2 modulo 3
-- * Add 3 to each number
addMod3Is2 :: [Int] -> [Int]
addMod3Is2 xs = [ i + 3 | i <- xs , mod i 3 == 2 ]





-- * Combine all elements in a list of lists into a single list
concat :: [[a]] -> [a]
concat xss = [ x | xs <- xss , x <- xs ]





(@#$%) :: Int -> Int -> Int
x@#$%y = x + 2 * y





(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False




fac :: Int -> Int
fac n = case n of
    0 -> 1
    n -> n * fac (n-1)





isort :: [Int] -> [Int]
isort []     = []
isort (x:xs) = insert x (isort xs)
  where
    insert :: Int -> [Int] -> [Int]
    insert x []     = [x]
    insert x (y:ys)
      | x <= y      = x:y:ys
      | otherwise   = y:(insert x ys)






isSorted :: Ord a => [a] -> Bool
isSorted xs =                   -- xs  = [1    ,3    ,2]
    let ys = drop 1 xs          -- ys  = [3    ,2    ]
        xys = zip xs ys         -- xys = [(1,3),(3,2)]
        prop (x,y) = x <= y     --       [1<=3 ,3<=2 ]
                                --       [True ,False]
    in  all prop xys            --       False




prop_isort_isSorted :: [Int] -> Bool
prop_isort_isSorted xs = isSorted (isort xs)
