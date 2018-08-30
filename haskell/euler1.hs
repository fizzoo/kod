import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 1000) $ testGroup "all"
  [ testProperty "statey" $ \x -> (x :: Integer) > 1 ==> smartsol x == statefulsol x
  , testProperty "compy" $ \x -> (x :: Integer) > 1 ==> smartsol x == compsol x
  , testProperty "docompy" $ \x -> (x :: Integer) > 1 ==> smartsol x == compbutindo x
  , testProperty "triple" $ \x -> (x :: Integer) > 1 ==> smartsol x == triplelist x
  , testProperty "rec" $ \x -> (x :: Integer) > 1 ==> smartsol x == recsol x
  , testProperty "accrec" $ \x -> (x :: Integer) > 1 ==> smartsol x == accrecsol x
  ]

corrmult :: Integral a => a -> Bool
corrmult x = x `mod` 3 == 0 || x `mod` 5 == 0

smartsol :: Integral a => a -> a
smartsol n = sum $ filter corrmult [1..n-1]

compsol :: Integral a => a -> a
compsol n = sum [i | i <- [1..n-1], corrmult i]

compbutindo :: Integral a => a -> a
compbutindo n = sum $ do
  i <- [1..n-1]
  guard $ corrmult i
  return i

triplelist :: (Num a, Enum a) => a -> a
triplelist n = sum [0, 3..n-1] + sum [0, 5..n-1] - sum [0, 15..n-1]

recsol :: Integral t => t -> t
recsol 0 = 0
recsol n = toadd + recsol n'
  where
    n' = n-1
    toadd = if corrmult n' then n' else 0

accrecsol :: Integral t => t -> t
accrecsol n = accrecsol' (n-1) 0

accrecsol' :: Integral t => t -> t -> t
accrecsol' 0 acc = acc
accrecsol' n acc = accrecsol' (n-1) (acc+toadd)
  where
    toadd = if corrmult n then n else 0

statefulsol :: Integral a => a -> a
statefulsol n = runST $ do
  acc <- newSTRef (0 :: Int)
  cur <- newSTRef (1 :: Int)

  replicateM_ (fromIntegral n - 1) $ do
    c <- readSTRef cur
    modifySTRef acc (if corrmult c then (+c) else id)
    modifySTRef cur (+1)

  s <- readSTRef acc
  return (fromIntegral s)
