module RayTracer where

import Data.Vec3
import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Function

import Hitable
import Vectors
import World



maxFloat :: Double
maxFloat = fromIntegral $ snd $ floatRange (0.5::Double)


(*.) :: Double -> CVec3 -> CVec3
k *. v = fromXYZ (x * k, y * k, z * k)
  where (x, y, z) = toXYZ v

-- y :: Vec3 a -> Double
y v = y'
  where (_, y', _) = toXYZ v




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



sky :: Ray -> Color
sky r = ((1.0 - t) *. CVec3 1 1 1) <+> (t *. CVec3 0.5 0.7 1.0)
  where un = normalize $ direction r
        t = (y un + 1) * 0.5

traceColorK :: RandomGen g => Double -> [Hitable_] -> Ray -> Rand g Color
traceColorK k objects r = case (hit objects r 0.00001 maxFloat) of
  -- Just (Hit t p n) -> 0.5 *. (CVec3 1 1 1 <+> n)
  Just (Hit t p n) ->
    do ru <- randomInUnitBall
       let target = p <+> n <+> ru
       cl <- traceColorK (k/2) objects $ Ray p (target <-> p)
       return cl
  Nothing -> return $ mapv (k*) $ sky r


traceColor :: RandomGen g => [Hitable_] -> Ray -> Rand g Color
traceColor objects r = traceColorK 1 objects r
-- traceColor _ r = return $ sky r
