module Geometry.Camera where

import           Data.Vec3

import           Types
import           Config

camera = mkCamera (CVec3 13 2 3)
                  (CVec3 0 0 0)
                  (CVec3 0 (-1) 0)
                  20
                  (fromIntegral (fst res) / fromIntegral (snd res))
                  10

mkCamera :: CVec3 -> CVec3 -> CVec3 -> Double -> Double -> Double -> Camera
mkCamera from at vup vfov aspect focusDist = Camera
  { cLowerLeftCorner = from
    <-> (u .^ (halfWidth * focusDist))
    <-> (v .^ (halfHeight * focusDist))
    <-> (w .^ focusDist)
  , cHorizontal      = u .^ (2 * halfWidth * focusDist)
  , cVertical        = v .^ (2 * halfHeight * focusDist)
  , cOrigin          = from
  }
 where
  theta      = vfov * pi / 180
  halfHeight = tan $ theta / 2
  halfWidth  = aspect * halfHeight
  w          = normalize $ from <-> at
  u          = normalize $ vup >< w
  v          = w >< u

getRay :: Camera -> Double -> Double -> Ray
getRay c u v = Ray
  (cOrigin c)
  (   cLowerLeftCorner c
  <+> ((cHorizontal c) .^ u)
  <+> ((cVertical c) .^ v)
  <-> (cOrigin c)
  )
