
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck


data Tree a = Leaf a
            | Node (Tree a) (Tree a)
  deriving (Show, Eq)


instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTree
      where
        genTree = oneof [ genLeaf , genNode ]
        genLeaf = fmap Leaf arbitrary
        genNode = pure Node <*> genTree <*> genTree
            --let genNodeFun = fmap Node arbitrary -- :: Gen (Tree -> Tree)
            --in  genNodeFun <*> arbitrary

    shrink (Leaf x) = []
    shrink (Node l r) = 
        [l, r] ++ 
        [Node l' r | l' <- shrink l] ++
        [Node l r' | r' <- shrink r] 


size :: Tree a -> Int
size (Leaf _) = 1
size (Node l r) = 1 + size l + size r

prop_isSmall :: Tree Int -> Bool
prop_isSmall t = size t <= 5




main :: IO ()
main = do
    putStrLn "Welcome to TODO app!"
    pickAnOption []

  where
    pickAnOption :: [String] -> IO ()
    pickAnOption todos = do
        putStrLn "Please pick an option:"
        putStrLn "1. Add item"
        putStrLn "2. Remove an item"
        putStrLn "3. Print the full list"
        putStrLn "4. Quit"
        input <- getLine
        case input of
            "1" -> addItem todos
            "2" -> removeItem todos
            "3" -> showItems todos
            "4" -> return ()
            _   -> do
                putStrLn "Invalid input, please try again!"
                pickAnOption todos

    addItem :: [String] -> IO ()
    addItem todos = do
        putStrLn "What item to add?"
        input <- getLine
        pickAnOption (todos ++ [input])

    removeItem :: [String] -> IO ()
    removeItem todos = do
        putStrLn "Which item to remove?"
        i <- readLn :: IO Int
        pickAnOption (take (i-1) todos ++ drop i todos)

    showItems :: [String] -> IO ()
    showItems todos = do
        _ <- traverse putStrLn todos
        pickAnOption todos
