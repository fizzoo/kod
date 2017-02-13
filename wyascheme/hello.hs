module Main where
import           Control.Monad
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

stringInside :: Parser Char
stringInside = (oneOf "\\" >> oneOf "\"nrt\\") <|> noneOf "\""

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many stringInside
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  nr <- many1 digit
  return $ (Number . read) nr

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> Either String LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> Left $ "No match: " ++ show err
    Right val -> Right val

main :: IO ()
main = do
         (expr:_) <- getArgs
         case (readExpr expr) of
           Left str  -> putStrLn str
           Right val -> print val
