import           Data.List

-- Return the dot product of a and b, assuming that the lists are of equal length
dot :: Num a => [a] -> [a] -> a
dot a b = sum $ zipWith (*) a b

-- Return the matrix multiplication of a and b, assuming that the matrices have alright dimensions
matmul :: Num a => [[a]] -> [[a]] -> [[a]]
matmul a b = map go a
  where
    b' = transpose b
    go x = map (dot x) b'

-- Return Nothing if the arg is not a well formed rectangular matrix, and otherwise Just (nrrows, nrcols)
matrixDim :: [[a]] -> Maybe (Int, Int)
matrixDim [[]] = Nothing
matrixDim mat = if all (\x -> length x == nr_cols) (tail mat) then Just (nr_rows, nr_cols) else Nothing
  where
    nr_rows = length mat
    nr_cols = length (head mat)

-- If it is an alright matmul, return Just the new matrix, otherwise Nothing
safeMatmul :: Num a => [[a]] -> [[a]] -> Maybe [[a]]
safeMatmul a b = case matrixDim a of
  Nothing -> Nothing
  Just (_, a_cols) -> case matrixDim b of
    Nothing          -> Nothing
    Just (b_rows, _) -> if a_cols == b_rows then Just (matmul a b) else Nothing

throwingMatmul :: Num a => [[a]] -> [[a]] -> [[a]]
throwingMatmul a b = case safeMatmul a b of
  Nothing -> error "Matrix mult with bad dims"
  Just a  -> a

main :: IO ()
main = do
  print $ matmul [[1, 2]] [[1], [2]]
  print $ matmul [[1], [2]] [[1, 2]]

  print $ safeMatmul [[1, 2]] [[1], [2]]
  print $ safeMatmul [[1], [2]] [[1, 2]]

  print $ matmul [[1, 2, 3]] [[1], [2]]
  print $ matmul [[1, 2]] [[1, 2]]

  print $ safeMatmul [[1, 2, 3]] [[1], [2]]
  print $ safeMatmul [[1, 2]] [[1, 2]]
