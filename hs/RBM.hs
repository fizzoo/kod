{-# LANGUAGE TemplateHaskell #-}

module RBM where

import           Control.Lens          hiding ((<.>), (|>))
import           Control.Monad.State
import           Data.Foldable
import           Numeric.LinearAlgebra
import           System.Random


type Input = [Bool]

toBool :: Functor f => f Double -> f Bool
toBool = fmap (> 0.5)

inp1 :: [Input]
inp1 = toBool <$>
  [[1, 0, 1, 0, 1, 0]
  ,[1, 1, 1, 0, 0, 0]]

data RBM = RBM { _weights :: Matrix Double, _vbias :: Vector Double, _hbias :: Vector Double } deriving (Show)
makeLenses ''RBM

nrVisible :: Getter RBM Int
nrVisible = to (rows . view weights)

nrHidden :: Getter RBM Int
nrHidden = to (cols . view weights)

nullrbm :: Int -> Int -> RBM
nullrbm visible hidden = RBM ((hidden >< visible) zeros) (visible |> zeros) (hidden |> zeros)
  where
    zeros = repeat 0.0

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

energy :: RBM -> Vector Double -> Vector Double -> Double
energy rbm v h = - a - b - c
  where
    a = v <.> (rbm ^. vbias)
    b = h <.> (rbm ^. hbias)
    c = (h <# (rbm ^. weights)) <.> v

-- | Requires binary data, i.e. v binary, for mathematical correctness
freeEnergyBin :: RBM -> Vector Double -> Double
freeEnergyBin rbm v = let
  b = rbm ^. vbias
  c = rbm ^. hbias
  w = rbm ^. weights
  lhs = b <.> v
  rhs = sumElements $ 1 + exp (c + w #> v)
  in -lhs -rhs

addUniformNoise :: Matrix R -> R -> State StdGen (Matrix R)
addUniformNoise m hi = do
  let (i,j) = size m
  rngs <- nRandoms $ i * j
  let rngmat = ((i><j) rngs) * 2 - 1
  return $ rngmat + m

goodNoiseParam rbm = 4 * sqrt (6 / fromIntegral (rbm ^. nrHidden + rbm ^. nrVisible))

main :: IO ()
main = do
  rng <- getStdGen
  let rbm = nullrbm 6 4
      res = foldlM train rbm inp1
  return ()


test1 = nullrbm 4 5
test2 = test1 & weights %~ (+1)

testrng = do
  rng <- getStdGen
  let neww = evalState (addUniformNoise (test1 ^. weights) (goodNoiseParam test1)) rng
  return $ test1 & weights .~ neww

testv :: Vector R
testv = build 4 id

testh :: Vector R
testh = build 5 (+5)

-- energy test2 testv testh

