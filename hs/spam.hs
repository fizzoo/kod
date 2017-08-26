
import           Data.Char


main :: IO ()
main = getLine >>= mapM_ putStrLn . createList

createList :: String -> [String]
createList x = take (length s) (iterate (fstUpper . transpose) s)
  where
    s = fixSpace x

fstUpper :: String -> String
fstUpper (x:xs) = toUpper x : map toLower xs
fstUpper []     = error "Not good"

transpose :: String -> String
transpose (x:xs) = xs ++ [x]

fixSpace :: String -> String
fixSpace x = if last x == ' ' then x else x ++ " "
