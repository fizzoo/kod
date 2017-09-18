module Ramanujan where

import           Data.List

-- Get all ramanujans w/ a,b,c,d <= n
rama :: (Num t, Enum t, Eq t) => t -> [(t, (t, t, t, t))]
rama n = [(a*a*a + b*b*b, (a,b,c,d)) |
          a <- [1..n], b <- [a..n],
          c <- [a..n], d <- [c..n],
          a*a*a + b*b*b == c*c*c + d*d*d,
          a /= c, b /= d, a /= d]

-- Get ramanujans relevant for raman n, i.e. a,b,c,d below cube root of n
ramaf :: (Integral a) => a -> [(a, (a, a, a, a))]
ramaf n = rama . ceiling . (** (1/3)) . fromIntegral $ n

-- Check if n is a ramanujan number
raman :: Int -> Bool
raman n = any ((==n) . fst) $ ramaf n
