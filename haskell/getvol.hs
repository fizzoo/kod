-- Simple thing to get the current master volume, is easily done in pure bash but did as test of shelly
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Shelly
import qualified Data.Text as T
default (T.Text)

main :: IO ()
main = (shelly . silently) getVolAlsa >>= print . mean . extractPercentages

mean :: [Int] -> Int
mean lst = sum lst `div` length lst

extractPercentages :: T.Text -> [Int]
extractPercentages inp = let
  l = filter (T.any (=='%')) (T.lines inp)
  getperc str = T.takeWhileEnd (\x -> x >= '0' && x <= '9') (fst $ T.break (=='%') str)
  in map (parseInt . getperc) l

parseInt :: T.Text -> Int
parseInt = read . T.unpack

getVolAlsa :: Sh (T.Text)
getVolAlsa = run "amixer" ["get", "Master"]

