rec :: (Integer -> Double) -> Integer -> Integer -> Double -> Double -> Double
rec f maxn n prevmax prevdiff
  | n > maxn = prevdiff
  | otherwise = rec f maxn (n+1) (max cur prevmax) (max diff prevdiff)
  where
    cur = f n
    diff = prevmax - cur


artichokes :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Double
artichokes p a b c d n = rec price n 1 (price 1) 0
  where
    price k = pf * (sin(af * fromInteger k + bf) + cos(cf * fromInteger k + df) + 2)
    pf = fromInteger p :: Double
    af = fromInteger a :: Double
    bf = fromInteger b :: Double
    cf = fromInteger c :: Double
    df = fromInteger d :: Double


main :: IO ()
main = do
  line <- getLine
  let ints   = map read $ words line :: [Integer]
      answer = case ints of [p, a, b, c, d, n] -> artichokes p a b c d n
                            _ -> error "Invalid parameters"
  print answer
