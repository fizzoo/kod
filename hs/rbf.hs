{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TemplateHaskell           #-}
import           Control.Lens          hiding ((<.>))
import           Control.Monad.State
import           Numeric.LinearAlgebra
import           System.Random

data RBF = RBF { _position :: Matrix R, _stddev :: Vector R, _weights :: Matrix R, _eta :: R }

makeLenses ''RBF

type STR a = State (RBF, StdGen) a

rbf :: (Functor f, Field1 s t a b) => (a -> f b) -> s -> f t
rbf = _1

rng :: (Functor f, Field2 s t a b) => (a -> f b) -> s -> f t
rng = _2

distance :: Vector R -> R -> Vector R -> R
distance position stddev x = normalization * exponent
  where
    dims = size position
    diff = x - position
    normalization = 2 * pi * det cov
    exponent = exp (-0.5 * ((diff <# cov) <.> diff))
    cov = ident dims * scalar stddev



relu :: (Num a, Ord a) => a -> a
relu x = if x <= 0 then 0 else x

delta :: Matrix R -> Vector R -> Vector R -> R -> Matrix R
delta w x t eta = w + scalar eta * (x `outer` (t - out))
  where
    out :: Vector R
    out = tr w #> x

trainpercept :: Vector R -> Vector R -> STR ()
trainpercept x t = do
  r <- use rbf
  rbf.weights .= delta (r ^. weights) x t (r ^. eta)
  return ()

fwd
  :: (Field1 s s RBF RBF, MonadState s m) => Vector R -> m (Vector R)
fwd x = do
  w <- use $ rbf.weights
  return $ tr w #> x

nRandoms :: Int -> STR (Vector R)
nRandoms n = do
  vector <$> replicateM n getone
  where
    getone = do
      r <- use rng
      let (nr, r') = randomR (0.0, 1.0) r
      rng .= r'
      return nr

rngrbf :: STR ()
rngrbf = undefined

xors :: [(Vector R, Vector R)]
xors = [([0,0], [0]), ([0,1], [1]), ([1,0], [1]), ([1,1], [0])] & traverse.both %~ vector

ands :: [(Vector R, Vector R)]
ands = [([0,0], [0]), ([0,1], [0]), ([1,0], [0]), ([1,1], [1])] & traverse.both %~ vector

nobiasands :: [(Vector R, Vector R)]
nobiasands = ands & traverse._1 -~ 0.7

testa :: IO ()
testa = do
  let rbf = RBF {_weights = (2><1) [0, 0], _eta = 0.1}
  rng <- newStdGen
  let s = flip evalState (rbf, rng) $ do
        sequenceA ( concat . replicate 10 $ uncurry trainpercept <$> nobiasands) 
        ans <- sequenceA (fwd <$> nobiasands ^.. traverse._1)
        return ans
  
  print s
    
