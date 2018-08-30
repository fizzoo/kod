

bern :: Double -> (Bool -> Double)
bern mu = \x -> case x of
  True  -> mu
  False -> 1 - mu


factorial :: (Num t, Eq t) => t -> t
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1)

binomial :: Integral t => t -> t -> t
binomial x y = factorial x `div` (factorial (x-y) * factorial y)

bin :: Integer -> Double -> (Integer -> Double)
bin n mu = \m -> fromIntegral (binomial n m) * mu^m * (1-mu)^(n-m)


beta :: Double -> Double -> Double -> Double
beta a b = \mu -> (gamma (a+b) / (gamma a * gamma b)) * mu**(a-1) * (1-mu)**(b-1)


-- https://en.wikipedia.org/wiki/Lanczos_approximation, python
-- solution. way off though.
gammapy :: Double -> Double
gammapy z = let
    looped = sum [pval / (z+i) | (i, pval) <- zip [0..] constants]
    x = 0.99999999999980993 + looped :: Double
    constants =
      [ 676.5203681218851, -1259.1392167224028, 771.32342877765313
      , -176.61502916214059, 0.507343278686905, -0.13857109526572012
      , 0.9843695780195716e-6, 1.5056327351493116e-7]
    t = z + fromIntegral (length constants) - 1.5
    in sqrt(2*pi) * t**(z-0.5) * exp(-t) * x

--- https://wiki.haskell.org/Gamma_and_Beta_function
cof :: [Double]
cof = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]

ser :: Double
ser = 1.000000000190015

gammaln :: Double -> Double
gammaln xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                 ser' = ser + (sum $ zipWith (/) cof [xx+1..])
             in -tmp' + log(2.5066282746310005 * ser' / xx)


gamma :: Double -> Double
gamma = exp . gammaln
