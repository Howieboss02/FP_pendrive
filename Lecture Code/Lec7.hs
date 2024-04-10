module Lec7 where

import Test.QuickCheck

-- The module Parsing.Parsing is included in the project
import Parsing.Parsing

import Data.Char






catchError :: Either err a -> (err -> Either err a) -> Either err a
catchError (Left err) h = h err
catchError (Right x)  _ = Right x 

safeDiv :: Int -> Int -> Either String Int
safeDiv x y
  | y == 0    = Left "division by 0"
  | otherwise = return (div x y)

test :: Either String Int
test = do
  x <- safeDiv 5 0
  return (x + 1)

test2 :: Either String Int
test2 = catchError test (\err -> return 42)

-- >>> test2
-- Right 42






pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x,y)

-- >>> pairs [1,2] "abc"
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]

myBindList :: [a] -> (a -> [b]) -> [b]
myBindList xs f = concat (map f xs)



{- This is the type of boolean expressions we want to parse -}
data BoolExpr
    = Atom String | Tru | Fls
    | And   BoolExpr BoolExpr
    | Or    BoolExpr BoolExpr
    | Not   BoolExpr
  deriving (Show, Eq)

atomExpr :: Parser BoolExpr
atomExpr = do
  var <- some (sat isAlpha)
  return (Atom var)

trueExpr :: Parser BoolExpr
trueExpr = do
  _ <- string "true"
  return Tru

falseExpr :: Parser BoolExpr
falseExpr = do
  _ <- string "false"
  return Fls

andExpr :: Parser BoolExpr
andExpr = do
  e1 <- boolExpr1
  _  <- token (string "&&")
  e2 <- boolExpr
  return (And e1 e2)

orExpr :: Parser BoolExpr
orExpr = do
  e1 <- boolExpr1
  _  <- token (string "||")
  e2 <- boolExpr
  return (Or e1 e2)

notExpr :: Parser BoolExpr
notExpr = do
  _ <- token (string "~")
  e1 <- boolExpr1
  return (Not e1)

parenExpr :: Parser BoolExpr
parenExpr = do
  _ <- token (string "(")
  e1 <- boolExpr
  _ <- token (string ")")
  return e1

boolExpr1 :: Parser BoolExpr
boolExpr1 = parenExpr <|> notExpr <|> trueExpr <|> falseExpr <|> atomExpr

boolExpr :: Parser BoolExpr
boolExpr = andExpr <|> orExpr <|> boolExpr1

-- >>> parse boolExpr ""
-- []

-- >>> parse boolExpr "x"
-- [(Atom "x","")]

-- >>> parse boolExpr "true"
-- [(Tru,"")]

-- >>> parse boolExpr "false"
-- [(Fls,"")]

-- >>> parse boolExpr "x && y"
-- [(And (Atom "x") (Atom "y"),"")]

-- >>> parse boolExpr "x && y && z"
-- [(And (Atom "x") (And (Atom "y") (Atom "z")),"")]

-- >>> parse boolExpr "x || y"
-- [(Or (Atom "x") (Atom "y"),"")]

-- >>> parse boolExpr "x && y || z"
-- [(And (Atom "x") (Or (Atom "y") (Atom "z")),"")]

-- >>> parse boolExpr "~ x"
-- [(Not (Atom "x"),"")]

-- >>> parse boolExpr "~ ~ x"
-- [(Not (Not (Atom "x")),"")]

-- >>> parse boolExpr "~ x && x"
-- [(And (Not (Atom "x")) (Atom "x"),"")]

-- >>> parse boolExpr "(true)"
-- [(Tru,"")]

-- >>> parse boolExpr "~ ( x && ~ x )"
-- [(Not (And (Atom "x") (Not (Atom "x"))),"")]









data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f r) (fmap f l)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = oneof [ Leaf <$> arbitrary , uncurry Node <$> arbitrary2 ]

    shrink (Leaf x) = map Leaf (shrink x)
    shrink (Node l r) = [ l , r ] ++ [ Node l' r | l' <- shrink l ] ++ [ Node l r' | r' <- shrink r]

prop_functor_id :: Tree Int -> Bool
prop_functor_id t = fmap id t == t

prop_functor_compose :: Fun Int Int -> Fun Int Int -> Tree Int -> Bool
prop_functor_compose (Fn f) (Fn g) t = fmap (f . g) t == fmap f (fmap g t)




--  \x y -> (x,y)           ::                         a -> b -> (a,b)
--  (,)                     ::                         a -> b -> (a,b)
--  Just (,)                ::                  Maybe (a -> b -> (a,b))
--  pure (,)                :: Applicative f => f     (a -> b -> (a,b))
--  pure (,) <*> xs         :: Applicative f => f          (b -> (a,b))
-- (pure (,) <*> xs) <*> ys :: Applicative f => f                (a,b)


--    (pure (,) <*> Just 1) <*> Just 2
-- == (Just (,) <*> Just 1) <*> Just 2
-- == (Just ((,) 1))        <*> Just 2
-- == Just ((,) 1 2)
-- == Just (1,2)

