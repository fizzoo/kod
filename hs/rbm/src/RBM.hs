{-# LANGUAGE TemplateHaskell #-}

module RBM where

import           Control.Lens          hiding ((<.>), (|>))
import           Control.Monad.State
import           Data.Foldable
import           Numeric.LinearAlgebra
import           System.Random


type Input = Vector R

toBool :: Functor f => f Double -> f Bool
toBool = fmap (> 0.5)

sign :: (Ord a, Num a, Num t) => a -> t
sign x
  | x >= 0 = 1
  | otherwise = -1

data RBM = RBM { _weights :: Matrix R, _vbias :: Vector R, _hbias :: Vector R } deriving (Show)
makeLenses ''RBM

nrVisible :: Getter RBM Int
nrVisible = to (rows . view weights)

nrHidden :: Getter RBM Int
nrHidden = to (cols . view weights)

nullrbm :: Int -> Int -> RBM
nullrbm visible hidden = RBM ((hidden >< visible) zeros) (visible |> zeros) (hidden |> zeros)
  where
    zeros = repeat 0.0


-- rngrbm :: Int -> Int -> Double -> State StdGen RBM
rngrbm visible hidden rnghi = do
  let nulled = nullrbm visible hidden
      addv = flip addUniformNoise  rnghi
      addm = flip addUniformNoiseM rnghi
  vis <- addv $ nulled ^. vbias
  hid <- addv $ nulled ^. hbias
  wei <- addm $ nulled ^. weights
  return $ nulled & vbias .~ vis & hbias .~ hid & weights .~ wei



sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp(-x))

sigmoidvec :: Vector R -> Vector R
sigmoidvec x = 1 / (1 + cmap exp (-x))

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

addUniformNoise :: Vector R -> R -> State StdGen (Vector R)
addUniformNoise v hi = do
  rngs <- nRandoms $ size v
  let rngv = (size v |> rngs) * 2 - 1
  return $ scalar hi * rngv + v

addUniformNoiseM :: Matrix R -> R -> State StdGen (Matrix R)
addUniformNoiseM m hi = do
  let (i,j) = size m
  rngs <- nRandoms $ i * j
  let rngm = ((i><j) rngs) * 2 - 1
  return $ scalar hi * rngm + m

-- | random parameters that deeplearning.net used for initializing weights
goodNoiseParam :: Floating a => RBM -> a
goodNoiseParam rbm = 4 * sqrt (6 / fromIntegral (rbm ^. nrHidden + rbm ^. nrVisible))

-- | Probability of hidden nodes firing given sample v
pHid :: RBM -> Input -> Vector R
pHid rbm v = sigmoidvec $ b' + (w #> v)
  where
    b' = rbm ^. hbias
    w = rbm ^. weights

-- | Probability of visible nodes firing given sample h
pVis :: RBM -> Vector R -> Vector R
pVis rbm h = sigmoidvec $ v' + (h <# w)
  where
    v' = rbm ^. vbias
    w = rbm ^. weights

testfire :: Vector R -> State StdGen (Vector R)
testfire probs = do
  rngs <- nRandoms $ size probs
  return $ cmap sign $ probs - vector rngs

main :: IO ()
main = do
  rng <- getStdGen
  let rbm = nullrbm 6 4
      res = foldlM train rbm inp1
  return ()


test1 = nullrbm 4 5
test2 = test1 & weights %~ (+1)


testv :: Vector R
testv = build 4 id

testh :: Vector R
testh = build 5 (+5)

inp1 :: [Input]
inp1 = vector . fmap (sign . subtract 0.5) <$>
  [[1, 0, 1, 0, 1, 0]
  ,[1, 1, 1, 0, 0, 0]]

-- energy test2 testv testh

-- | positivevb???
-- cdPos :: Vector R -> Vector R -> Int -> Vector R
-- cdPos v h n = vector . fmap ((/fromIntegral n) . sumElements) . toRows $ v `outer` h

-- | thought they wanted at least some sum?! but just the outer, which doesn't need a function
-- compCD :: Vector R -> Vector R -> Int -> R
-- compCD v h n = (/fromIntegral n) . sumElements $ v `outer` h

compCDs v h n = 
  where
    e = v `outer` h
    evb = 

test :: IO ()
test = do
  rng <- getStdGen
  print . flip runState rng $ do
      testrng <- rngrbm 6 4 0.01
      let dims = length inp1
          v = head inp1
          ph = pHid testrng v
      h1 <- testfire ph
      let cdpos = v `outer` h1 / (scalar $ fromIntegral dims)
          pv = pVis testrng h1
      v1 <- testfire pv
      h2 <- testfire (pHid testrng v)
      v2 <- testfire (pVis testrng h2)
      let cdneg = v2 `outer` h2 / (scalar $ fromIntegral dims)
      return (cdpos,cdneg)
