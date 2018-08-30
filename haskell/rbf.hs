{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
import           Control.Lens          hiding ((<.>), (|>))
import           Control.Monad.State
import           Numeric.LinearAlgebra
import Data.List (sortBy)
import           System.Random

data RBF = RBF { _position :: Matrix R, _stddev :: Vector R, _weights :: Matrix R, _eta :: R } deriving (Show)

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
delta w x t eta = scalar eta * (x `outer` (t - out))
  where
    out :: Vector R
    out = tr w #> x

sgn
  :: (Num b, Num a, Ord a, Container c a, Element b) => c a -> c b
sgn = cmap (\x -> if x >= 0 then 1 else -1)

trainpercept :: Vector R -> Vector R -> STR ()
trainpercept x t = do
  r <- use rbf
  rbf.weights += delta (r ^. weights) (addbias x) t (r ^. eta)
  return ()

fwd
  :: (Field1 s s RBF RBF, MonadState s m) => Vector R -> m (Vector R)
fwd x = do
  w <- use $ rbf.weights
  return $ tr w #> addbias x

addbias :: Vector R -> Vector R
addbias x = vjoin [x, scalar 1]

nRandoms :: Int -> STR (Vector R)
nRandoms n = do
  vector <$> replicateM n getone
  where
    getone = do
      r <- use rng
      let (nr, r') = randomR (0.0, 1.0) r
      rng .= r'
      return nr

nullrbf inp hid out eta = RBF { _position = (hid><inp) z
                              , _stddev = hid |> z
                              , _weights = ((hid+1)><out) z
                              , _eta = eta}
  where
    z = repeat 0

addbiasv :: R -> Vector R -> STR (Vector R)
addbiasv hi v = do
  rng <- nRandoms $ fromIntegral $ size v
  return $ (rng*2 - 1)* scalar hi

addbiasm :: R -> Matrix R -> STR (Matrix R)
addbiasm hi m = do
  let r = rows m
      c = cols m
  rng <- nRandoms $ r*c
  return $ reshape c $ (rng*2 - 1)* scalar hi

rngrbf :: R -> STR ()
rngrbf hi = do
  let apply :: (Container c R) => (R -> c R -> STR (c R)) -> Lens' (RBF,StdGen) (c R) -> STR ()
      apply f l = do use l >>= f hi >>= (l .=)
  apply addbiasm (rbf.position)
  apply addbiasv (rbf.stddev)
  apply addbiasm (rbf.weights)
  return ()

xors :: [(Vector R, Vector R)]
xors = [([0,0], [0]), ([0,1], [1]), ([1,0], [1]), ([1,1], [0])] & traverse.both %~ vector & traverse._2 %~ sgn . (subtract 0.5)

ands :: [(Vector R, Vector R)]
ands = [([0,0], [0]), ([0,1], [0]), ([1,0], [0]), ([1,1], [1])] & traverse.both %~ vector & traverse._2 %~ sgn . (subtract 0.5)

shuffle :: [a] -> STR [a]
shuffle x = do
  r <- nRandoms $ length x
  return $ snd <$> sortBy (\x y -> fst x `compare` fst y) (zip (toList r) x)

testa :: IO ()
testa = do
  let rbfn = nullrbf 2 2 1 0.1
  rng <- newStdGen
  let s = flip evalState (rbfn, rng) $ do
        rngrbf 0.01
        replicateM 100 $ do
          a <- shuffle ands
          sequenceA $ uncurry trainpercept <$> a
        ans <- sequenceA (fwd <$> ands ^.. traverse._1)
        w <- use $ rbf.weights
        return (ans,w)
  
  print s
    
