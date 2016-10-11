{-# LANGUAGE GADTs #-}

module Sorty (sorty, unsorty, Sorty ()) where

import Test.QuickCheck
import Data.Monoid ((<>))

data Sorty a where
  Sorty :: Ord a => [a] -> Sorty a

sorty :: Ord a => [a] -> Sorty a
sorty a = Sorty (sort a)

unsorty :: Sorty a -> [a]
unsorty (Sorty a) = a

instance Ord a => Monoid (Sorty a) where
  mempty = Sorty []
  mappend (Sorty x) (Sorty y) = Sorty $ mergy x y

instance Eq a => Eq (Sorty a) where
  (Sorty a) == (Sorty b) = listEqual a b
    where
    listEqual [] [] = True
    listEqual _ [] = False
    listEqual [] _ = False
    listEqual (a:as) (b:bs) = a == b && listEqual as bs

instance Show a => Show (Sorty a) where
  show (Sorty a) = "Sorty " ++ show a

somap :: (a -> a) -> Sorty a -> Sorty a
somap f (Sorty x) = Sorty $ sort $ map f x

mergy :: Ord a => [a] -> [a] -> [a]
mergy [] b = b
mergy a [] = a
mergy (a:aa) (b:bb) = if a < b then a : mergy aa (b:bb) else b : mergy (a:aa) bb

sort :: Ord a => [a] -> [a]
sort [] = []
sort [a] = [a]
sort x = mergy (sort (take (div l 2) x)) (sort (drop (div l 2) x))
  where
  l = length x

sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _ = True

prop_sort_sorted :: [Int] -> Bool
prop_sort_sorted = sorted . sort

prop_sorty_sorted :: [Int] -> Bool
prop_sorty_sorted a = sorted b 
  where
  (Sorty b) = sorty a

prop_sorty_retains_data :: [Int] -> Bool
prop_sorty_retains_data x = length x == length y
  where
  (Sorty y) = sorty x

prop_unsorty_sorty_id :: [Int] -> Bool
prop_unsorty_sorty_id x = sort x == (unsorty . sorty $ x)

prop_sorty_sorted_plus :: [Int] -> [Int] -> Bool
prop_sorty_sorted_plus x y = sorted z
  where
  (Sorty z) = sorty x `mappend` sorty y

prop_sorty_monoid_laws1 :: [Int] -> Bool
prop_sorty_monoid_laws1 x = mappend mempty s == s && mappend s mempty == s
  where
  s = sorty x
prop_sorty_monoid_laws2 :: [Int] -> [Int] -> [Int] -> Bool
prop_sorty_monoid_laws2 x y z = mappend s1 (mappend s2 s3) == mappend (mappend s1 s2) s3
  where
  s1 = sorty x
  s2 = sorty y
  s3 = sorty z
