{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Lib where

import           Control.Lens
import           Control.Monad.State
import           Data.List             (intersperse)
import qualified Data.Text             as T
import           Data.Text.Lens
import           Test.Tasty
import           Test.Tasty.QuickCheck



-- Short introduction to haskell, showcasing some interesting features.  Done
-- as part of a lunch&learn, so to a reasonably senior audience it should fit
-- into a 45 min presentation (Though not complete understanding, just a
-- taste).


main :: IO ()
main = undefined

















-- auto-infer
add :: Num a => a -> a -> a
add x y = (x + y)

-- multiply_nat :: Word -> Word -> Word
-- multiply_nat :: (Num t, Ord t) => t -> t -> t
-- multiply_nat :: (Num t1, Num t2, Ord t2) => t1 -> t2 -> t1
-- multiply_nat :: (Num t1, Num t2, Ord t2) => t1 -> t2 -> t1
multiply_nat x y = f x y 0
  where
    f a n acc = if n <= 0 then acc else f a (n-1) (acc+a)

-- Don't need to use currying really
add'' :: (Int, Int) -> Int
add'' (x, y) = x + y

add' :: (Int, Int) -> Int
add' = uncurry add


















-- | fib
fib_naive n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fib_naive (n-1) + fib_naive (n-2)

fib_naive' 0 = 1
fib_naive' 1 = 1
fib_naive' n = fib_naive' (n-1) + fib_naive' (n-2)


-- fib_acc :: Int -> Int
fib_acc number = fib_acc' number 1 1
  where
    fib_acc' n acc1 acc2
      | n <= 0 = acc1
      | otherwise = fib_acc' (n-1) acc2 (acc1+acc2)

-- lazy
to_ten1 = [1..10]
to_ten2 = take 10 [1..10000]
to_ten3 = take 10 [1..]
to_ten_even = takeWhile (<=10) $ map (*2) [1..]
to_ten_even' = takeWhile (<=10) (map (*2) [1..])


fib_list = (fibs !!)

fibs = map fib' [0..]
  where
    fib' 0 = 1
    fib' 1 = 1
    fib' n = fibs !! (n-1) + fibs !! (n-2)

















-- | hole-driven development
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f1 f2 = \x -> f1 (f2 x)


-- compose' f1 f2 = \x -> f1 $ f2 x
-- compose' f1 f2 = f1 . f2













-- | balanced parenthesis
balanced str = balanced' str 0
balanced' (x:xs) n
  | n < 0 = False
  | x == '(' = balanced' xs (n+1)
  | x == ')' = balanced' xs (n-1)
  | otherwise = balanced' xs n
balanced' [] n = n == 0

paren_to_val c
  | c == '(' = 1
  | c == ')' = -1
  | otherwise = 0

balanced_helpf str = f str 0
  where
    f (x:xs) n
      | n < 0 = False
      | otherwise = f xs (n + paren_to_val x)
    f [] n = n == 0

-- seems like a fold
-- :i foldr/l

my_sum list = foldr (+) 0 list
my_prod list = foldr (*) 1 list

my_min (x:xs) = foldr min x xs

balanced_fold_fn :: (Int, Bool) -> Char -> (Int, Bool)
balanced_fold_fn (n, b) c = (n + paren_to_val c, b && n >= 0)

balanced_foldr str = b && n == 0
  where
    (n, b) = foldl balanced_fold_fn (0, True) str

balanced_better [] = True
balanced_better str = let
  ls = scanl (+) 0 (map paren_to_val str)
  in last ls == 0 && all (>= 0) ls


















-- | datatypes
data Tree a = Branch (Tree a) (Tree a) | Leaf a
  deriving (Show)

tree_to_list :: Tree a -> [a]
tree_to_list (Branch a b) = tree_to_list a ++ tree_to_list b
tree_to_list (Leaf a)     = [a]

list_to_right_tree :: [a] -> Tree a
list_to_right_tree []     = error "No empty trees"
list_to_right_tree [a]    = Leaf a
list_to_right_tree (x:xs) = Branch (Leaf x) (list_to_right_tree xs)

-- | gadt

data Tree' a where
  Branch' :: Tree' a -> Tree' a -> Tree' a
  Leaf' :: a -> Tree' a















-- | property based tests

equal_result
  :: (Eq a, Ord t, Num t) => (t -> a) -> (t -> a) -> t -> Property
equal_result f1 f2 x = x >= 0 && x <= 20 ==> f1 x == f2 x

equal_result_int :: Eq a => (Int -> a) -> (Int -> a) -> Int -> Property
equal_result_int = equal_result

runtest = defaultMain

prop_fibs_equally_good = let
  -- zero cost of help-functions usually, so might as well use it to
  -- reduce typing
  tp s f1 f2 = testProperty s $ equal_result_int f1 f2
  in testGroup "fibs"
     [ tp "acc" fib_naive fib_acc
     , tp "list" fib_naive fib_list
     ]

















-- :info String

split_first :: (a -> Bool) -> [a] -> ([a], [a])
split_first f xs = let
  left = takeWhile f xs
  right = dropWhile f xs
  in (left, right)

split :: Char -> String -> [String]
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
  where
  (xs', xs'') = split_first (/=c) xs

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]

prop_encdec_split = testProperty "split" (\xs -> forAll (Test.Tasty.QuickCheck.elements xs) $ \c -> unsplit c (split c xs) == xs)

















-- class with custom serializer
data Person = Person { _name   :: String
                     , _age    :: Int
                     , _iscool :: Bool
                     } deriving (Show, Eq)

serialize_person :: Person -> String
serialize_person p = _name p ++ "," ++ show (_age p) ++ "," ++ show (_iscool p)

unserialize_person :: String -> Person
unserialize_person str = let
  separate = split ',' str
  in Person (separate !! 0) (read $ separate !! 1) (read $ separate !! 2)

-- complains nicely
prop_encdec_person = testProperty "split" (\p -> p == (unserialize_person . serialize_person $ p))

-- :i Arbitrary

instance Arbitrary Person where
  -- no need for type annotations
  arbitrary = do
    name' <- arbitrary
    age' <- choose (0, 100)
    iscool' <- arbitrary
    return $ Person name' age' iscool'
 -- note: in list monad :)
  shrink p = do
    shorter_name <- shrink (_name p)
    return $ Person shorter_name (_age p) (_iscool p)



















-- | (<$>, (<*>), (>>=) on lists/just/IO
data Maybe' a = Just' a | Nothing' deriving (Show)

class Functor' a where
  fmap' :: (a -> b) -> f a -> f b

instance Functor Maybe' where
  fmap f (Just' a) = Just' (f a)
  fmap f Nothing'  = Nothing'

print_stuff :: IO ()
print_stuff = do
  print 1
  print "hej"
  return ()

print_stuff' = print 1 >> print "hej"

answer = getLine >>= \x -> putStrLn $ x ++ "!!"

list_of_print :: [IO ()]
list_of_print = replicate 10 print_stuff

print_lots = sequence list_of_print













-- | leonardo

-- one nice thing about having first-class objects for all effects is
-- that they can be combined and often abstracted over in interesting
-- ways

-- see other files

















-- | lenses
anna = Person "Anna" 8  True
olle = Person "Olle" 12 False

data Cooperation = C { _p1 :: Person, _p2 :: Person } deriving (Show)

coop = C anna olle

-- really annoying, maybe the #1 reason haskell hasn't taken over the
-- business world
coop_but_fst_uncool (C (Person an aa ac) b) = C (Person an aa False) b


makeLenses ''Person
makeLenses ''Cooperation

-- :i view, ^.
-- coop^.p1.iscool


coop_but_snd_uncool :: Cooperation -> Cooperation
coop_but_snd_uncool c = p2.iscool .~ False $ c

coop_but_snd_is_fst :: Cooperation -> Cooperation
coop_but_snd_is_fst c = (p2 .~ (c^.p1) $ c)

l1 = mapped %~ (++"!") $ ["hej", "ja"]
l2 = (mapped._2) %~ (+3) $ [(1,2),(3,4)]

-- isomorphisms
hello :: T.Text
hello = "hello" ^. packed

hello_back :: String
hello_back = hello ^. from packed



imp_add_all :: State [Int] ()
imp_add_all = do
  traverse += 3
  ix 0 *= 100
  return ()


data Player = Player { _location  :: (Int, Int)
                     , _hitpoints :: Int } deriving Show
data Game = Game { _units :: [Player]
                 , _level :: Int} deriving Show
makeLenses ''Player
makeLenses ''Game

default_game = Game (replicate 3 $ Player (0,0) 100) 0


battle :: State Game ()
battle = do
  -- leader progressed a bit y-wise
  units.ix 0.location._2 += 4

  -- everyone moved a bit x-wise
  units.traversed.location._1 += 2

  -- people got hurt
  units.traversed.hitpoints -= 10


  -- people who stepped forward in y-axis got hit by some other attack
  -- accessor on steroids
  units.traversed.filtered (\x -> x^.location._2 > 1).hitpoints -= 33

  -- got experience
  level += 1
