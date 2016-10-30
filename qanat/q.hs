dig :: Floating a => a -> a
dig x = 0.5 * x * x
  
calculate_costs :: Floating a => a -> a -> [a] -> a
calculate_costs w h ls = rec (0:ls ++ [w])
  where
  heightmult = h / w
  rec (a:b:rest) = left + right + rec (b:rest)
    where
    midpoint = ((a+b)/2) + (heightmult*(b-a)/2)
    left = dig (midpoint-a + heightmult*a)
    right = dig (b-midpoint + heightmult*b) - dig (heightmult*b)
  rec [x] = dig (heightmult*x)
  rec _ = error "[] rec call"

qanat :: Floating a => a -> a -> a -> [a]
qanat w h n = cost:placements
  where
    cost = calculate_costs w h placements
    placements = [3.0]
  
main :: IO ()
main = do
  line <- getLine
  let ints   = map read $ words line
      answers = case ints of [a, b, c] -> qanat a b c
                             _ -> error "Invalid parameters"
  mapM_ print answers
