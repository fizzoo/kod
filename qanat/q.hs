dig :: Floating a => a -> a
dig x = 0.5 * x * x
  
calculate_costs :: Floating a => a -> [a] -> a
calculate_costs h ls = rec ls
  where
  rec (a:b:rest) = left + right + rec (b:rest)
    where
    midpoint = ((a+b)/2) + (h*(b-a)/2)
    left = dig (midpoint-a + h*a)
    right = dig (b-midpoint + h*b) - dig (h*b)
  rec [x] = dig (h*x)
  rec _ = error "[] rec call"

qanat :: Floating a => a -> a -> a -> [a]
qanat w h n = cost:placements
  where
    cost = w * w * calculate_costs (h/w) placements
    placements = 0 : (map (/w) [48.0, 108.0]) ++ [1]
  
main :: IO ()
main = do
  line <- getLine
  let ints   = map read $ words line
      answers = case ints of [a, b, c] -> qanat a b c
                             _ -> error "Invalid parameters"
  mapM_ print answers
