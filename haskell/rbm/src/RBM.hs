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

sign01 :: (Ord a, Num a, Num t) => a -> t
sign01 x
  | x >= 0 = 1
  | otherwise = 0

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
  return $ cmap sign01 $ probs - vector rngs

inpbook :: [Input]
inpbook = vector <$>
  [[1,1,1,0,0,0],[1,0,1,0,0,0],[1,1,1,0,0,0],[0,0,1,1,1,0], [0,0,1,1,0,0],[0,0,1,1,1,0]]

-- | The algorithm part of the book is sort of half on-line, just
-- generally broken and using weeird notation if they actually mean
-- something that would end up as something like a weight. The code,
-- instead, does a full batch thingy without a single mention of it,
-- since the code would work either way. Anyhow, a weight update
-- scheme that might work for one-input-at-a-time is available at
-- http://image.diku.dk/igel/paper/AItRBM-proof.pdf , which this
-- follows. All three weight diffs for this one input, as (pos - neg),
-- is returned.  I.e: dw (Matrix), dv (vector), dh (vector). No
-- assumptions on h, but that algo seems to imply that it maybe should
-- stay as a probability, and not as sampled. v' is newer than v, etc.
-- Should probably divide this by number of inputs (?).
compCDs :: Vector R -> Vector R -> Vector R -> Vector R -> (Matrix R, Vector R, Vector R)
compCDs v h v' h' = (dw, dv, dh)
  where
    dw = h `outer` v - h' `outer` v'
    dv = v - v'
    dh = h - h'

-- | gibbs starting on v, h from input unused (structured this way for ease of folding)
gibbsv :: RBM -> (Vector R, Vector R) -> State StdGen (Vector R, Vector R)
gibbsv rbm (v, _) = do
  h' <- testfire $ pHid rbm v
  v' <- testfire $ pVis rbm h'
  return (v', h')

getDiffs :: RBM -> Input -> State StdGen (Matrix R, Vector R, Vector R)
getDiffs rbm inp = do
  let v = inp
  h <- testfire $ pHid rbm v
  (v', h') <- foldM (\vh _ -> gibbsv rbm vh) (v, h) [1..1]
  return $ compCDs v h v' h'

train :: R -> RBM -> [Input] -> State StdGen RBM
train eta rbm inputs = do
  diffs <- mapM (getDiffs rbm) inputs
  let (dw, dv, dh) = foldr (\(a, b, c) (a', b', c') -> (a+a', b+b', c+c')) (0,0,0) diffs
      dims = fromIntegral $ length inputs
      scale = eta / dims
      newrbm = rbm & weights %~ (+ (dw * scalar scale))
                   & vbias %~ (+ (dv * scalar scale))
                   & hbias %~ (+ (dh * scalar scale))
  return newrbm

fwd :: RBM -> Input -> State StdGen (Vector R)
fwd rbm inp = do
  -- (v', h') <- gibbsv rbm (inp, inp)
  h' <- testfire $ pHid rbm inp
  return $ pVis rbm h'


test :: IO ()
test = do
  rng <- newStdGen
  let tren = train 0.01
      test = vector [0,0,0,1,1,0]
  print . flip evalState rng $ do
      t0 <- rngrbm 6 4 0.01

      t1 <- tren t0 inpbook
      t2 <- foldM tren t1 (replicate 4000 inpbook)
      t3 <- tren t2 inpbook
      out <- fwd t3 (head inpbook)
      outtest <- fwd t3 test
      return (out, outtest)


-- | We can see in the output of test that the probabilities after
-- forwarding the test vector on the one where it is active is very high,
-- often >0.7, which is completely unlike for (head inpbook), where
-- they are <0.2 (positions 4,5, that is). Similarly the first of the
-- training inputs fires higher probabilities on its activations, and
-- all this very consistently. If we keep gibbs-ing, the probabilities
-- tend towards the relative presence in the training set, which
-- asserts the generative-ness of it. Pos 3 is ~1 and pos 6 ~0 in all
-- cases, which is reasonable for our data.
--
-- Doing it batch-wise would probably be faster, doing persistent CD
-- likewise. Absolutely none of the improvements from hinton's very
-- reasonable practical guide are implemented, and it would improve
-- everything manyfold. (Also, the fold is pointless on the singleton
-- list, but it really seems to be the case that sampling once
-- performs the best)
--
-- As it is though, it seems to do some kind of learning!
