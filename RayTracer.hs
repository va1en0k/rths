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

--
-- color' :: RandomGen g => Ray -> Rand g Color
-- color' r = case (hit objects r 0 maxFloat) of
--   Just (Hit t p n) -> return $ 0.5 *. (CVec3 1 1 1 <+> n)
--   -- Just (Hit t p n) ->
--   --   do ru <- randomInUnitBall
--   --      let target = p <+> n <+> ru
--   --      cl <- color $ Ray p (target <-> p)
--   --      return $ 0.5 *. cl
--   Nothing -> return $ sky r
--
color :: RandomGen g => Ray -> Rand g Color
color r = case (hit objects r 0.00001 maxFloat) of
  -- Just (Hit t p n) -> 0.5 *. (CVec3 1 1 1 <+> n)
  Just (Hit t p n) ->
    do ru <- randomInUnitBall
       let target = p <+> n <+> ru
       cl <- color $ Ray p (target <-> p)
       return $ mapv (0.5 *) cl
  Nothing -> return $ sky r



traceColorK :: RandomGen g => [Hitable_] -> Int -> Ray -> Rand g Color
-- traceColorK _       pw ray | pw > 32 = return $ mapv (/ (2 ^ pw)) (sky ray)
traceColorK objects pw ray = case (hit objects ray 0.000001 maxFloat) of
  -- Just (Hit t p n) -> 0.5 *. (CVec3 1 1 1 <+> n)
  Just (Hit t p n) ->
    do ru <- randomInUnitBall
       let target = p <+> n <+> ru
       cl <- traceColorK objects (pw + 1) $ Ray p (target <-> p)
       return cl
  Nothing -> return $ mapv (/ (2 ^ pw)) (sky ray)

traceColor :: RandomGen g => [Hitable_] -> Ray -> Rand g Color
-- traceColor objects r = traceColorK objects 1 r
traceColor objects r = color r
-- traceColor _ r = return $ sky r
