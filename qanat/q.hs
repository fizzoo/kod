dig :: Floating a => a -> a -> a 
dig from to = 0.5 * to * to - 0.5 * from * from
  
calculate_costs :: Floating a => a -> a -> [a] -> a
calculate_costs w h ls = lefts ls + rights ls + begin + end
  where
  heightmult = w / h
  begin = dig 0 (head ls * (1.0 + heightmult)/ 2)
  end = dig 0 (((w - last ls)/2) + h)
  lefts (a:b:rest) = dig 0 ((b-a + heightmult*(b-a))/2) + lefts (b:rest)
  lefts _ = 0                   -- 1 or 0 elements
  rights (a:b:rest) = 0.0
  rights _ = 0                  -- 1 or 0 elements

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
