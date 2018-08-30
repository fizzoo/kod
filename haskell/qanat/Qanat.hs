module Qanat where

-- Very difficult primitive function.
dig :: Floating d => d -> d
dig x = 0.5 * x * x

-- Fully calculates costs of some placements of shafts.
calculateCosts :: Floating d => d -> [d] -> d
calculateCosts h = rec
  where
  rec (a:b:rest) = left + right + rec (b:rest)
    where
    midpoint = ((a+b)/2) + (h*(b-a)/2)
    left = dig (midpoint-a + h*a)
    right = dig (b-midpoint + h*b) - dig (h*b)
  rec [x] = dig (h*x)
  rec _ = error "[] rec call"

-- Esoteric closed form solution based on h,
-- gets the proportions right.
f :: (Floating d, Integral i) => d -> i -> d
f h x = let g = -1/(h**2 - 1)
            sq = sqrt(g**2 - 1)
            a = g + sq
            b = g - sq
            in a^x - b^x

-- Helper to also scale f. Curry it.
place :: (Floating d, Integral i) => d -> d -> i -> i -> d
place w h maxn = (/ f h maxn) . (w*) . f h

-- Computes solution to entire problem.
-- Returns both the cost sum and the <10 shaft positions.
qanat :: Int -> Int -> Int -> [Double]
qanat w h n = take 11 $ cost : tail (init placements)
  where
  cost = calculateCosts heightmult placements
  placements = map p [0..(n+1)]
  p = place wf heightmult (n+1)
  wf = fromIntegral w :: Double
  heightmult = fromIntegral h / wf :: Double

main :: IO ()
main = do
  line <- getLine
  let ints   = map read $ words line
      answers = case ints of [a, b, c] -> qanat a b c
                             _         -> error "Invalid parameters"
  mapM_ print answers
