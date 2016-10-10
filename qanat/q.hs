import Data.List.Split (splitOn)

dig :: Floating a => a -> a -> a 
dig from to = 0.5 * to * to - 0.5 * from * from
  
qanat :: Floating a => a -> a -> a -> [a]
qanat w h n = cost:placements
  where
    cost = dig 0 2.25 + dig 0 2.25 + dig 1.5 5.25 + dig 0 5.25
    g = w/3.0
    placements = [3.0]
  
main :: IO ()
main = do
  line <- getLine
  let intstr = splitOn " " line
      ints   = map read intstr
      answers = case ints of [a, b, c] -> qanat a b c
                             _ -> error "Invalid parameters"
  mapM_ print answers
