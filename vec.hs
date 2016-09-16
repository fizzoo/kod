import qualified Data.Vector.Unboxed         as V
import           System.Random               (randomIO)


main :: IO ()
main = do
    vector <- V.generateM (16*16) gen
    print vector
      where
        gen :: a -> IO Bool
        gen = const randomIO
