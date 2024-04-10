module Lec8 where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

{-
fac 3 
= 3 * fac 2
= 3 * (2 * fac 1)
= 3 * (2 * 1 * fac 0)
= 3 * (2 * (1 * 1))  
= 6
-}


fac' :: Integer -> Integer
fac' n = acc 1 n
  where
  acc :: Integer -> Integer -> Integer
  acc x 0 = x
  acc x y = (acc $! (x*y)) (y-1)

-- >>> fac' 1000000
-- *** Exception: ProgressCancelledException

{-

Call by name:

fac' 3
= acc 1 3
= acc (1 * 3) 2
= acc ((1 * 3) * 2) 1
= acc (((1 * 3) * 2) * 1) 0
= ((1 * 3) * 2) * 1
= (3 * 2) * 1
= 6 * 1
= 6

Call by value:

fac' 3
= acc 1 3
= acc (1 * 3) 2
= acc 3 2
= acc (3 * 2) 1
= acc 6 1
= acc (6 * 1) 0
= acc 6 0
= 6
-}



multOf2Or3 :: [Integer]
multOf2Or3 = merge23 multOf2 multOf3
  where 
    multOf2 = 4 : map (+2) multOf2  -- map (*2) [2..]
    multOf3 = 6 : map (+3) multOf3  -- [ 3*k | k <- [2..] ]

    merge23 :: [Integer] -> [Integer] -> [Integer]
    merge23 (x:xs) (y:ys)
      | x < y     = x : merge23 xs (y:ys)
      | x == y    = x : merge23 xs ys
      | otherwise = y : merge23 (x:xs) ys
    merge23 _ _ = undefined

-- >>> take 10 multOf2Or3
-- [4,6,8,9,10,12,14,15,16,18]



merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys) 
  | x == y    = x : merge xs ys
  | otherwise = y : merge (x:xs) ys

(\\) :: Ord a => [a] -> [a] -> [a]
[] \\ _ = []
xs \\ [] = xs
(x:xs) \\ (y:ys) 
  | x < y = x : (xs \\ (y:ys))
  | x == y = xs \\ (y:ys)
  | otherwise = (x:xs) \\ ys


-- Labeling trees
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

split :: [a] -> ([a],[a])
split (y:z:xs) =
  let (ys,zs) = split xs
  in  (y:ys , z:zs)
split xs = (xs,[])

labelTree :: [Int] -> Tree a -> Tree (Int,a)
labelTree xs Leaf = Leaf
labelTree [] (Node l y r) = error "Not enough labels!"
labelTree (x:xs) (Node l y r) = 
  let (ys,zs) = split xs
  in Node (labelTree ys l) (x,y) (labelTree zs r)



-- >>> let myTree = Node (Node Leaf 'a' Leaf) 'b' (Node (Node Leaf 'c' Leaf) 'd' Leaf)
-- >>> labelTree [1..] myTree
-- Node (Node Leaf (2,'a') Leaf) (1,'b') (Node (Node Leaf (5,'c') Leaf) (3,'d') Leaf)


-- Cut off branches of the tree beyond a given depth
cutoffTree :: Int -> Tree a -> Tree a
cutoffTree 0 _ = Leaf
cutoffTree n Leaf = Leaf
cutoffTree n (Node l x r) = 
  Node (cutoffTree (n-1) l) x (cutoffTree (n-1) r)

-- >>> let infTree = Node infTree () infTree
-- >>> cutoffTree 3 (labelTree [1..] infTree)
-- Node (Node (Node Leaf (4,()) Leaf) (2,()) (Node Leaf (6,()) Leaf)) (1,()) (Node (Node Leaf (5,()) Leaf) (3,()) (Node Leaf (7,()) Leaf))



-- First implementation of primes
primesV1 :: [Integer]
primesV1 = sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve [] = []
    sieve (x:xs) = 
      let xs' = [ y | y <- xs, y `mod` x /= 0 ]
      in  x : sieve xs'

-- 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : ...
-- ^
-- 2 : 3 :     5 :     7 :     9 : ....
--     ^
-- 2 : 3 :     5 :     7 :         ...   
--             ^

-- >>> take 10 primesV1
-- [2,3,5,7,11,13,17,19,23,29]

-- >>> primesV1 !! 10000
-- 104743





-- Another way of computing prime numbers:
-- First compute the infinite list of all
-- *composite* (non-prime) numbers, and then
-- remove these numbers from the list [2..]

multiples :: [[Integer]]
multiples = [ [n*n,n*(n+1)..] | n <- [2..] ]

{- Here is a representation of the infinite
list of infinite lists `multiples`:

4   6   8   10  12  ...    [2,4..]
9   12  15  18  21  ...    [3,6..]
16  20  24  28  32  ...    [4,8..]
25  30  35  40  45  ...
36  42  48  54  60  ...
... ... ... ... ... ...

-}

-- [4] ++ merge [6,8..] [9,12,15,16,18...] 

-- Merge together an infinite list of infinite lists
-- Assumes that all lists are increasing.
mergeAll :: Ord a => [[a]] -> [a]
mergeAll []           = []
mergeAll ([]    :xss) = mergeAll xss
mergeAll ((x:xs):xss) = x : merge xs (mergeAll xss)


-- >>> take 10 (mergeAll multiples)



composites :: [Integer]
composites = mergeAll multiples

-- >>> take 10 (mergeAll multiples)
-- [4,6,8,9,10,12,14,15,16,18]


primesV2 :: [Integer]
primesV2 = [2..] \\ composites

-- >>> take 10 primesV2
-- [2,3,5,7,11,13,17,19,23,29]

-- >>> primesV2 !! 10000
-- 104743


-- Can we go faster? Yes: we can only consider
-- the lists [p*p,p*(p+1),p*(p+2),...]
-- where p is a prime number!
primesV3 :: [Integer]
primesV3 = 2 : ([3..] \\ composites)
  where
    composites = mergeAll primeMultiples
    primeMultiples = [ [p*p,p*(p+1)..] | p <- primesV3 ]

-- >>> take 10 primesV3
-- [2,3,5,7,11,13,17,19,23,29]

-- >>> primesV3 !! 10000
-- 104743
