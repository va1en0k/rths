module RayTracer where

import Data.Vec3
import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Function

import Hitable
import Vectors
import World
import Random


maxFloat :: Double
maxFloat = fromIntegral $ snd $ floatRange (0.5::Double)



-- y :: Vec3 a -> Double
y v = y'
  where (_, y', _) = toXYZ v



sky :: Ray -> Color
sky r = ((1.0 - t) *. CVec3 1 1 1) <+> (t *. CVec3 0.5 0.7 1.0)
  where un = normalize $ direction r
        t = (y un + 1) * 0.5

traceColorK :: RandomGen g => Int -> Double -> [Hitable_] -> Ray -> Rand g Color
traceColorK i k objects r = case (hit objects r 0.00001 maxFloat) of
  -- Just (Hit t p n) -> 0.5 *. (CVec3 1 1 1 <+> n)
  Just (Hit t p n) ->
    if i < 50
      then do
            ru <- randomInUnitBall
            let target = p <+> n <+> ru
            cl <- traceColorK (i+1) (k/2) objects $ Ray p (target <-> p)
            return cl
      else return $ CVec3 0 0 0
  Nothing -> return $ mapv (k*) $ sky r


traceColor :: RandomGen g => [Hitable_] -> Ray -> Rand g Color
traceColor objects r = traceColorK 0 1 objects r
-- traceColor _ r = return $ sky r
