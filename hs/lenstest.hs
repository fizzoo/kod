{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import qualified Data.Map             as Map
import           Data.Matrix

mys :: (MonadState Int m, MonadIO m) => m ()
mys = do
  cur <- get
  modify (+1)
  cur2 <- get
  liftIO $ putStrLn $ "Was " ++ show cur ++ ", now " ++ show cur2
  if cur < 5 then mys else return ()


compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f $ g x


myr :: Reader [Int] Int
myr = do
  cur <- ask
  case cur of
    [x]    -> return x
    (x:xs) -> local (const xs) myr


my :: RWS [Int] [Int] Int Int
my = do
  (curinp:remainder) <- ask
  cursum <- get
  let added = curinp + cursum
  tell $ return added
  put added
  case remainder of
    [] -> return added
    x  -> local (const x) my


f1 :: State Int ()
f1 = modify (\x -> x - 1)

f :: State Int Int
f = do
  f1
  f1
  get



data Marriage = M { _wife :: Person, _husb :: Person } deriving (Show)
data Person = P { _name :: String, _salary :: Int, _addr :: String } deriving (Show)

olle = P "Olle" 12 "y"
anna = P "Anna" 8  "e"

mar = M anna olle

makeLenses ''Person
makeLenses ''Marriage

allp :: Traversal' Person String
allp elt (P n s a) = (\n' a' -> P n' s a') <$> (elt n) <*> (elt a)


-- view wife mar
-- view (wife . name) mar

-- type Degrees = Double
-- type Latitude = Degrees
-- type Longitude = Degrees

-- data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) }
-- makeLenses ''Meetup


mat = matrix 5 7 (uncurry (+))

mapdiag :: (a -> a) -> Matrix a -> Matrix a
mapdiag f m = let
  d = min (nrows m) (ncols m)
  in foldl (\m i -> setElem (f (m ! (i,i))) (i,i) m) m [1..d]

-- diag :: Lens' (Matrix a) a
-- diag :: forall f a. Functor f => (a -> f a) -> Matrix a -> f (Matrix a)
-- diag :: Functor f => (Matrix b -> f b) -> Matrix b -> f (Matrix b)
-- diag :: Traversal' (Matrix a) a
-- diag = traverse mapdiag


atx :: forall f a. Functor f =>
     (Int, Int) -> (a -> f a) -> Matrix a -> f (Matrix a)
atx pos elt m = wrap <$> (elt mv)
  where
    mv = m ! pos

    wrap :: a -> Matrix a
    wrap x = setElem x pos m

atm k mb_fn m = wrap <$> (mb_fn mv)
  where
    mv = Map.lookup k m

    wrap (Just v') = Map.insert k v' m
    wrap Nothing = case mv of Nothing -> m
                              Just _  -> Map.delete k m

mapp = Map.fromList [(1,"hej"), (2, "da")]

