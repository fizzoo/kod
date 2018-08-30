module Eul2 where


import           Data.Array
import           Data.Function


fib :: Int -> Integer
fib = (fibs !!)

fibs :: [Integer]
fibs = map fib' [0..]
  where
    fib' 0 = 1
    fib' 1 = 1
    fib' n = fibs !! (n-1) + fibs !! (n-2)

main :: IO ()
main = print $ sum $ takeWhile (< 4000000) (filter even fibs)

-- testing https://wiki.haskell.org/Memoization

fibf :: (Int -> Integer) -> Int -> Integer
fibf _ 0 = 1
fibf _ 1 = 1
fibf f n = f (n-1) + f (n-2)


memoize :: (Int -> a) -> Int -> a
memoize f = (map f [0..] !!)

fib2 :: Int -> Integer
fib2 = fix (memoize . fibf)


-- Article had a good idea, but it seems a new memo array was created
-- every call! Similar in vein to "eta-expansion destroys
-- memoization", we cannot give it an explicit parameter, and the
-- guard simply seems to want one. When lifted to top-level (trivial,
-- didn't even have a free variable) it performs approximately
-- 'wondrousMemoSize' times as fast. (Tested with -O2 also)

wondrous :: Integer -> Integer
wondrous x
  | x <= wondrousMemoSize = memodWondrous ! x
  | otherwise = wondrous' x

wondrousMemoSize :: Integer
wondrousMemoSize = 512

memodWondrous :: Array Integer Integer
memodWondrous = array (1, wondrousMemoSize) [ (i, wondrous' i) | i <- [1..wondrousMemoSize]]

wondrous' :: Integer -> Integer
wondrous' 1 = 0
wondrous' x
    | even x = 1 + wondrous (x `div` 2)
    -- Article used prime version here, guessing that it would cache
    -- miss too often. I found no measurable speedup, and straight up
    -- corecursion is easier to remember.
    | otherwise = 1 + wondrous (3*x+1)

wondrousnomem :: Integer -> Integer
wondrousnomem 1 = 0
wondrousnomem x
    | even x = 1 + wondrousnomem (x `div` 2)
    | otherwise = 1 + wondrousnomem (3*x+1)

d2 :: Array (Integer, Integer) Integer
d2 = array ((1,1), (10,10)) [((i,j), i+j) | i <- [1..10], j <- [1..5]] -- half undefined
