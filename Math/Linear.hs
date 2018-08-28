module Math.Linear where

import Data.Matrix

-- Ax = B
-- x = A^-1 * B
solve :: (Fractional a, Eq a) => [[a]] -> [a] -> [a]
solve a b = case (* fromLists (map return b)) <$> inverse (fromLists a) of
  Right a -> toList a
  Left err -> error err

-- Data.Matrix> solve [[1,3,(-2)], [3,5,6], [2,4,3]] [5, 7, 8]
-- [-15.0,8.0,2.0]
