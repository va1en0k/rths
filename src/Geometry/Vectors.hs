module Geometry.Vectors where

import           Linear.Vector
import           Linear.V3
import           Linear.Metric
import           Data.Ord
import           Data.List
import           Control.DeepSeq
import           Codec.Picture
import           Control.Monad.Random

import           Types

vsum :: [CVec3] -> CVec3
vsum = foldr1 (+)

minv :: [CVec3] -> CVec3
minv = minimumBy (comparing norm)

maxv :: [CVec3] -> CVec3
maxv = maximumBy (comparing norm)

avgv :: [CVec3] -> CVec3
avgv xs = mapv (/ fromIntegral (length xs)) (vsum xs)

mapv :: (Double -> Double) -> CVec3 -> CVec3
mapv f (V3 x y z) = V3 (f x) (f y) (f z)

-- instance NFData CVec3 where
--   rnf (V3 r g b) = r `seq` g `seq` b `seq` ()

-- instance NFData PixelRGB8 where
--   rnf (PixelRGB8 r g b) = r `seq` g `seq` b `seq` ()

(*.) :: Double -> CVec3 -> CVec3
(*.) = (*^)

(*<>*) :: CVec3 -> CVec3 -> CVec3
(V3 a b c) *<>* (V3 x y z) = V3 (a * x) (b * y) (c * z)
