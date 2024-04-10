import Test.QuickCheck
import Prelude hiding (maximum)

dist :: Int -> Int -> Int
dist x y = abs (x - y)

prop_dist_self :: Int -> Bool
prop_dist_self x = dist x x == 0

prop_dist_sym :: Int -> Int -> Bool
prop_dist_sym x y = dist x y == dist y x

prop_dist_pos :: Int -> Int -> Bool
prop_dist_pos x y = dist x y > 0





isSorted :: Ord a => [a] -> Bool
isSorted (x:y:xs) | x <= y = isSorted (y:xs)
                  | otherwise = False
isSorted (x:xs) = True
isSorted [] = True

prop_isSorted :: [Int] -> Bool 
prop_isSorted xs = isSorted xs



intersect :: [Int] -> [Int] -> [Int]
intersect xs ys = [ x | x <- xs , elem x ys ]







prop_luhn_correct :: Property
prop_luhn_correct = forAll gen (\xs -> length xs > 0 ==> luhn xs == luhnSpec xs)
  where
    gen = listOf (chooseInt (0,9))




data Nat = Zero | Suc Nat
  deriving Show

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n 
  | n > 0     = Suc (int2nat (n-1))
  | otherwise = error "negative input to int2nat"

maximum :: Nat -> Nat -> Nat
maximum Zero y = y
maximum x Zero = x
maximum (Suc x) (Suc y) = Suc (maximum x y)


data NonEmpty a =  a :| [a]

neToList :: NonEmpty a -> [a]
neToList ((:|) x xs) = x : xs



data Prop
  = PTrue
  | PFalse
  | PNot Prop
  | PVar String
  -- TODO: add more constructors

type Env = [(String,Bool)]

eval :: Env -> Prop -> Bool
eval env PTrue = True
eval env PFalse = False
eval env (PNot x) = not (eval env x)
eval env (PVar x) = case lookup x env of
  (Just b) ->  b
  Nothing  -> error "variable undefined"

pretty :: Prop -> String
pretty PTrue = "True"
pretty PFalse = "False"
pretty (PNot x) = "~(" ++ pretty x ++ ")"
pretty (PVar x) = x

allVars :: Prop -> [String]
allVars PTrue = []
allVars PFalse = []
allVars (PNot x) = allVars x
allVars (PVar x) = [x]

-- allEnvs ["x","y"] = [[("x",True),("y",True)], ...]

allEnvs :: [String] -> [Env]
allEnvs [] = [ [] ]
allEnvs (x:xs) = 
  let envs = allEnvs xs
  in  [ (x,b):env | b <- [True,False], env <- envs ] 

isTautology :: Prop -> Bool
isTautology p =
  let vars = allVars p
      envs = allEnvs vars
  in  and [ eval env p | env <- envs ]




































-- forAll (listOf (chooseInt (0,9))) 
--      (\xs -> length xs > 0 ==> luhn xs == luhnSpec xs)











luhn :: Integral a => [a] -> Bool
luhn xs | ok xs = luhnSum (reverse xs) `mod` 10 == 0
  where
    ok xs = all (\x -> x >= 0 && x < 10) xs && length xs > 0

    luhnDouble x = if double_x > 9 then double_x - 9 else double_x
      where double_x = 2*x

    luhnSum [] = 0
    luhnSum [x] = x
    luhnSum (x1:x2:xs) = x1 + luhnDouble x2 + luhnSum xs
luhn xs = error "bad input!"

luhnSpec :: Integral a => [a] -> Bool
luhnSpec xs = luhnSum (reverse xs) `mod` 10 == 0
  where
    luhnDouble x = if double_x > 9 then double_x - 9 else double_x
      where double_x = 2*x

    luhnSum [] = 0
    luhnSum [x] = x
    luhnSum (x1:x2:xs) = x1 + luhnDouble x2 + luhnSum xs


