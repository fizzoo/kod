-- Just testing out tasty and how one might use it for both hunit and
-- quickcheck (also smallcheck but i eventually found that quickcheck was
-- better in most cases, so it is not left here). As such, some of the
-- props are just explorations of the system.

import           Qanat                 hiding (main)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 1000) $ testGroup "All" [props, units]

close :: (Ord t, Fractional t) => t -> t -> Bool
close x y =  diff < 0.0001 || diff < 0.0001 * abs x
  where
    diff = abs (x - y)

closelist :: (Ord t, Fractional t) => [t] -> [t] -> Bool
closelist x y = all (uncurry close) (zip x y)

closetest :: (Show t, Ord t, Fractional t) => [t] -> [t] -> Assertion
closetest x y = closelist x y @? "Not close: " ++ show x ++ " vs " ++ show y

units :: TestTree
units = testGroup "units"
  [ testCase "kattis1" $ closetest (qanat 8 4 1) [31.5, 3.0]
  , testCase "kattis2" $ closetest (qanat 195 65 2) [12220.000000, 48.000000, 108.000000]
  , testCase "kattis3" $ closetest (qanat  10000 1 1000) [30141.885677, 9.956721, 19.913443, 29.870164, 39.826887, 49.783610,
                                                          59.740334, 69.697060, 79.653786, 89.610515, 99.567245]
  ]

data QanatInstance = Q Int Int Int deriving (Show)

instance Arbitrary QanatInstance where
  arbitrary = do
    w <- choose (1, 10000)
    h <- choose (1, w-1)
    n <- choose (1, 1000)
    return $ Q w h n

-- 100% for laziness/readability, shorter lists below
t :: Testable a => TestName -> a -> TestTree
t = testProperty

props :: TestTree
props = testGroup "props"
  [ t "Correct length" $ \(Q a b c) -> length (qanat a b c) == (if c > 10 then 11 else fromIntegral c + 1)
  -- The few NaN cases are too big for kattis too, so will ignore them
  , t "Positive" $ \(Q a b c) -> let r = qanat a b c in isNaN (head r) || all (>0) r
  -- I thought this might fail, but it does not. Surely not a good prop, but interesting.
  , t "bigger than b" $ \(Q a b c) -> let r = qanat a b c in isNaN (head r) || head r > fromIntegral b
  -- Expanding on previous prop, with high enough c the cost seems to be 0.8*a*b < cost < a*b.
  , t "weird bound" $ \(Q a b c) -> let r = head $ qanat a b c in
                                      not (isNaN r) && 3*c > a && 3*c > b ==> r < fromIntegral (a*b) && r > fromIntegral (a*b) * 0.8
  ]
