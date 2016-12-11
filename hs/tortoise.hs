import           Data.Char (toLower)


main = undefined

data Token = TForw | TBack | TLeft | TRight | TDown | TUp | TColor | TRep | TQuote | TDot | TInvalid | Tint Int | THex [Char]
  deriving (Show)

data TokenLine = TokenLine Token Int
  deriving (Show)

type Row = Int

num :: Char -> Bool
num x = x >= '0' && x <= '9'

nums :: [Char] -> Bool
nums a = all num a

hex :: Char -> Bool
hex x = num x || x >= 'a' && x <= 'f'

hexs :: [Char] -> Bool
hexs (h:t) = h == '#' && length t == 6 && all hex t
hexs []    = False

tokenize :: [Char] -> Token
tokenize l
  | l == "forw" = TForw
  | l == "back" = TBack
  | l == "left" = TLeft
  | l == "right" = TRight
  | l == "down" = TDown
  | l == "up" = TUp
  | l == "color" = TColor
  | l == "rep" = TRep
  | l == "\"" = TQuote
  | l == "." = TDot
  | nums l = Tint (read l)
  | hexs l = THex l
  | otherwise = TInvalid

untilComment :: [Char] -> [Char]
untilComment (x:xs) = if x == '%' then "" else x : untilComment xs
untilComment ""     = ""

tokenizeAll :: [Char] -> [TokenLine]
tokenizeAll inp = let
  l = map untilComment $ lines $ map toLower inp
  w = map goodWords l
  t = map (map tokenize) w
  tl = zipWith (\x y -> map (\x' -> TokenLine x' y) x) t [1..]
  in concat tl

goodWords a = filter (not . null) $ concat $ map words $ goodWords' "" a

goodWords' acc (x:xs) = if elem x "\"." then acc : [x] : goodWords' "" xs else goodWords' (acc ++ [x]) xs
goodWords' acc "" = [acc]

