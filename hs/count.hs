import           Control.Monad.Writer
import           Data.List.Split
import           Data.Monoid
import           Data.Set             hiding (filter, map)
import           System.Directory

type Dirrer = WriterT (Set FilePath) IO ()

push :: FilePath -> Dirrer
push name = tell $ singleton name

stem :: String -> String
stem "" = ""
stem x  = last . filter (/= "") $ splitOn "/" x

isParent :: FilePath -> Bool
isParent file = file == "." || file == ".."

trav :: FilePath -> Dirrer
trav dir = do
  let unstem = ((dir ++ "/") ++)

  names <- lift $ (map unstem . filter (not . isParent)) <$> getDirectoryContents dir
  files <- lift $ filterM doesFileExist names
  dirs  <- lift $ filterM doesDirectoryExist names

  mapM (push . stem) files
  mapM trav dirs

  return ()


main :: IO ()
main = do
  deduped <- execWriterT (trav ".")
  print $ size deduped
