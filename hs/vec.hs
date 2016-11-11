import           Data.Vector   ((!))
import qualified Data.Vector   as V
import           System.Random (randomIO)


main :: IO ()
main = do
  genrng
  print l10
  print $ dim10 ! 1 ! 1

genrng :: IO ()
genrng = do
  vector <- V.generateM (16*16) gen
  print vector
    where
      gen :: a -> IO Bool
      gen = const randomIO


l10 :: V.Vector Int
l10 = V.fromList [1..10]

dim10 :: V.Vector (V.Vector Int)
dim10 = V.replicate 10 $ V.fromList [1..10]

