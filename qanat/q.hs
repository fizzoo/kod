dig :: Floating a => a -> a
dig x = 0.5 * x * x

calculateCosts :: Double -> [Double] -> Double
calculateCosts h = rec
  where
  rec (a:b:rest) = left + right + rec (b:rest)
    where
    midpoint = ((a+b)/2) + (h*(b-a)/2)
    left = dig (midpoint-a + h*a)
    right = dig (b-midpoint + h*b) - dig (h*b)
  rec [x] = dig (h*x)
  rec _ = error "[] rec call"

placeBetween :: Floating a => a -> a -> a -> a
placeBetween a b h = -(a+b)*(h**2-1)/2

uniform :: Integer -> Double -> [Double]
uniform n max = map ((*(max/(fromInteger (n-1)))) . fromInteger) [0..(n-1)]

improveGuess :: Double -> [Double] -> Double -> [Double]
improveGuess prev (_:c:r) h = prev : improveGuess (placeBetween prev c h) (c:r) h
improveGuess prev [x] _ = prev:[x]
improveGuess _ _ _ = error "[] improveguess call"

lowdiff :: [Double] -> [Double] -> Bool
lowdiff a b = maximum (map abs (zipWith (-) a b)) < 0.00001

keepImproving :: [Double] -> Double -> [Double]
keepImproving prev h
  | lowdiff prev cur = cur
  | otherwise = keepImproving cur h
  where
    cur = improveGuess (head prev) (tail prev) h

qanat :: Integer -> Integer -> Integer -> [Double]
qanat w h n = cost : map (*wf) (tail $ init placements)
  where
  cost = wf * wf * calculateCosts heightmult placements
  placements = keepImproving (uniform (n+2) 1.0) heightmult :: [Double]
  wf = fromIntegral w :: Double
  heightmult = fromIntegral h / wf :: Double

main :: IO ()
main = do
  line <- getLine
  let ints   = map read $ words line
      answers = case ints of [a, b, c] -> qanat a b c
                             _ -> error "Invalid parameters"
  mapM_ print answers
