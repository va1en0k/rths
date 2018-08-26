module Random where

import           Control.Monad.Random
import           Data.Vec3

import           Geometry.Vectors
import           RTMonad
import           Types

randomInUnitBall' :: RT CVec3
randomInUnitBall' = do
  (x : y : z : r : _) <- getRands
  -- r <- getRandomR (-1::Double, 1)
  let p = r * sqrt (x * x + y * y + z * z)
  return $ (1 / p) *. CVec3 x y z


randomInUnitBall'' :: RT CVec3
randomInUnitBall'' = do
  do
    (a : b : c : _) <- getRands
    -- let p = (2.0 *. CVec3 a b c) <-> CVec3 1 1 1
    let p = CVec3 a b c
    if norm p >= 1 then randomInUnitBall'' else return p
  -- (x:y:z:r:_) <- getRands
  -- r <- getRandomR (-1::Double, 1)
  -- let p = r * sqrt (x*x + y*y + z*z)
  -- return $ (1 / p) *. CVec3 x y z

randomInUnitBall :: RandomGen g => Rand g CVec3
randomInUnitBall =
  -- do (a:b:c:_) <- getRandomRs (0::Double, 1)
  --    let p = (2.0 *. CVec3 a b c) <-> CVec3 1 1 1
  --    if norm p >= 1 then randomInUnitBall else return p
                   do --(x:y:z:_) <- normals
  (x : y : z : _) <- getRandomRs (-1 :: Double, 1)
  r               <- getRandomR (-1 :: Double, 1)
  let p = r * sqrt (x * x + y * y + z * z)
  return $ (1 / p) *. CVec3 x y z
