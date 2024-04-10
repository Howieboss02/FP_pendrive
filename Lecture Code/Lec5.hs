
import Test.QuickCheck
import Test.QuickCheck.Random





data TrafficLight = Red | Yellow | Green
  deriving (Eq, Show)

instance Arbitrary TrafficLight where
    arbitrary  = f <$> chooseInt (1,3)
      where
        f :: Int -> TrafficLight
        f 1 = Red
        f 2 = Yellow
        f 3 = Green
    shrink _ = []


-- prop> \x -> x == Green
-- *** Failed! Falsified (after 1 test):
-- Yellow

tester = quickCheckWith (stdArgs { replay = Just (mkQCGen 42, 0) }) (\x -> x == Red)

