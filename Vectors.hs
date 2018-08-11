module Vectors where

import Data.Vec3
import Data.Ord
import Data.List
import Control.DeepSeq
import Codec.Picture

type Color = CVec3


vsum :: [CVec3] -> CVec3
vsum = foldr1 (<+>)

minv :: [CVec3] -> CVec3
minv = minimumBy (comparing norm)

maxv :: [CVec3] -> CVec3
maxv = maximumBy (comparing norm)

avgv :: [CVec3] -> CVec3
avgv xs = mapv (/ fromIntegral (length xs)) (vsum xs)

mapv :: (Double -> Double) -> CVec3 -> CVec3
mapv f (CVec3 x y z) = CVec3 (f x) (f y) (f z)

instance NFData CVec3 where
  rnf (CVec3 r g b) = r `seq` g `seq` b `seq` ()

instance NFData PixelRGB8 where
  rnf (PixelRGB8 r g b) = r `seq` g `seq` b `seq` ()

reflect :: CVec3 -> CVec3 -> CVec3
-- might it be that it assumes that |n| = 1?
reflect v n = v <-> (2 *. ((v .* n) *. n))



(*.) :: Double -> CVec3 -> CVec3
(*.) = mapv . (*)
