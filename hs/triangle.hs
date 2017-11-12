-- | Generate list of lists containing 'take i [[1], [2,3], [4,5,6], ...]'
gen :: (Enum t, Num t) => t -> [[t]]
gen i = map (\(x, y) -> [x+1..y]) $ (\a -> zip a (tail a)) $ scanl (+) 0 [1..i]

-- | Show list of list of showables by putting a space inbetween each
-- element and a newline between the rows
showlist :: Show a => [[a]] -> IO ()
showlist l = sequence_ $ l >>= \a -> map (\a -> putStr (show a) >> putStr " ") a ++ [putStr "\n"]

main = do
  putStr "number of rows?:"
  s <- getLine
  showlist $ gen (read s)
