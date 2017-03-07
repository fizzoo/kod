{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import Control.Lens.TH
import Control.Lens

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
data Person = P { _name :: String, _salary :: Int } deriving (Show)

olle = P "Olle" 12
anna = P "Anna" 8

m = M anna olle

makeLenses ''Person
makeLenses ''Marriage

-- view wife m
-- view (wife . name) m

-- type Degrees = Double
-- type Latitude = Degrees
-- type Longitude = Degrees

-- data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) }
-- makeLenses ''Meetup
