module Normal
  ( normals
  )
where

import           Control.Monad.Random


-- Normal distribution approximation
-- ---------------------------------
-- | Box-Muller method for generating two normally distributed
-- independent random values from two uniformly distributed
-- independent random values.
boxMuller :: Floating a => a -> a -> (a, a)
boxMuller u1 u2 = (r * cos t, r * sin t)
 where
  r = sqrt (-2 * log u1)
  t = 2 * pi * u2


-- | Convert a list of uniformly distributed random values into a
-- list of normally distributed random values. The Box-Muller
-- algorithms converts values two at a time, so if the input list
-- has an uneven number of element the last one will be discarded.
boxMullers :: Floating a => [a] -> [a]
boxMullers (u1 : u2 : us) = n1 : n2 : boxMullers us
  where (n1, n2) = boxMuller u1 u2
boxMullers _ = []


normals :: (MonadRandom m, Random a, Floating a) => m [a]
normals = boxMullers <$> getRandoms
