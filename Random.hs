module Random where

import Control.Monad.Random
import Data.Vec3

import Vectors

randomInUnitBall :: RandomGen g => Rand g CVec3
randomInUnitBall =
  -- do (a:b:c:_) <- getRandomRs (0::Double, 1)
  --    let p = (2.0 *. CVec3 a b c) <-> CVec3 1 1 1
  --    if norm p >= 1 then randomInUnitBall else return p
  do --(x:y:z:_) <- normals
     (x:y:z:_) <- getRandomRs (0::Double, 1)
     r <- getRandomR (0::Double, 1)
     let p = r * sqrt (x*x + y*y + z*z)
     return $ (1 / p) *. CVec3 x y z
