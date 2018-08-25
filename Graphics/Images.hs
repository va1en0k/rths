module Graphics.Images where

import Codec.Picture
import Data.Array
import Data.Vec3
import Geometry.Vectors

import Types
import Util

type ImgBuf = Array (Int, Int) PixelRGB8

colorToPixel :: Color -> PixelRGB8
colorToPixel c =
  let (r, g, b) = toXYZ $ mapv ((* 255.9) . sqrt) c
  in PixelRGB8 (fromInteger $ floor r) (fromInteger $ floor g) (fromInteger $ floor b)
