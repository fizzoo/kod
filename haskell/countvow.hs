import Data.Char (toLower)

numberOfElems :: Char -> String -> Int
numberOfElems c = length . filter (c ==)

countVowels :: String -> String
countVowels str = show $ zip (vowels) (map elemcounter vowels)
  where
  vowels = "aouie"
  elemcounter = flip numberOfElems lowerStr
  lowerStr = map toLower str

main = interact countVowels
