{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import           Control.Monad.State
import           Data.Foldable
import           Data.Matrix         (Matrix (..), matrix, ncols, nrows)
import qualified Data.Vector.Unboxed as V
import           System.Random


type Input = [Bool]

toBool :: Functor f => f Double -> f Bool
toBool = fmap (> 0.5)

inp1 :: [Input]
inp1 = fmap toBool $
  [[1, 0, 1, 0, 1, 0]
  ,[1, 1, 1, 0, 0, 0]]

data RBM = RBM { _w :: Matrix Double, _vbias :: V.Vector Double, _hbias :: V.Vector Double }
makeLenses ''RBM

nrVisible :: Getter RBM Int
nrVisible = to (nrows . view w)

nrHidden :: Getter RBM Int
nrHidden = to (ncols . view w)

nullrbm :: Int -> Int -> RBM
nullrbm visible hidden = RBM (matrix visible hidden (const 0)) (nullvec visible) (nullvec hidden)
  where
    nullvec l = V.replicate l 0.0

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp(-x))

nRandoms :: Int -> State StdGen [Double]
nRandoms n = do
  replicateM n getone
  where
    getone = do
      rng <- get
      let (nr, rng') = randomR (0.0, 1.0) rng
      put rng'
      return nr

train :: RBM -> Input -> State StdGen RBM
train rbm inp = undefined



main :: IO ()
main = do
  rng <- getStdGen
  let rbm = nullrbm 6 4
      res = foldlM (\r i -> train r i) rbm inp1
  return ()
