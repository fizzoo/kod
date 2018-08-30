{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Writer
import           Data.Monoid
import           Data.Set             hiding (filter, map)
import           Data.Text            (Text, append, pack, splitOn, unpack)
import           System.Directory

type Dirrer = WriterT (Set Text) IO ()

push :: Text -> Dirrer
push name = tell $ singleton name

stem :: Text -> Text
stem "" = ""
stem x  = last . filter (/= "") $ splitOn "/" x

isParent :: Text -> Bool
isParent file = file == "." || file == ".."

trav :: Text -> Dirrer
trav dir = do
  let unstem = (append (append dir "/"))

  names <- lift $ (map unstem . filter (not . isParent) . map pack) <$> getDirectoryContents (unpack dir)
  files <- lift $ filterM (doesFileExist . unpack) names
  dirs  <- lift $ filterM (doesDirectoryExist . unpack) names

  mapM (push . stem) files
  mapM trav dirs

  return ()


main :: IO ()
main = do
  deduped <- execWriterT (trav ".")
  print $ size deduped
