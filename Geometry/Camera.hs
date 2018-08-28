module Geometry.Camera where

import           Data.Vec3

import           Types
import           Config

type Camera = (Double, Double) -> Ray



-- camera = Camera {
--   cOrigin = CVec3 13.0 4.0 19.0,
--   cHorizontal = CVec3 (-8.987280150604205) 0.0 4.987280150604205,
--   cVertical = CVec3 0.4806138049029842 (-6.4604193953014866) 0.4806138049029842, cLowerLeftCorner = CVec3 12.314843072425312 6.802851336421494 9.327562921821107
-- }

-- camera = Camera {cOrigin = CVec3 13.0 2.0 (-11.0), cHorizontal = CVec3 4.555881805991546 0.0 5.384223952535464, cVertical = CVec3 0.3140143177281695 (-3.502467390044967) (-0.26570442269306643), cLowerLeftCorner = CVec3 2.983297972382686 2.584810007982875 (-7.143929486203352)}

-- camera = Camera {
--   cOrigin = CVec3 0 0 (-11),
--   cHorizontal = CVec3 32 0 0,
--   cVertical = CVec3 0 16 0,
--   cLowerLeftCorner = CVec3 (-16) (-10) (-3)}

camera = mkCamera (CVec3 0 3 (-4))
                  (CVec3 0 0 0)
                  (CVec3 0 1 0)
                  20
                  (fromIntegral (fst res) / fromIntegral (snd res))
                  10

mkCamera :: CVec3 -> CVec3 -> CVec3 -> Double -> Double -> Double -> Camera
mkCamera from at vup fov aspect focusDist = \(u, v) ->
  Ray cOrigin
    (   cLowerLeftCorner
    <+> (cHorizontal .^ u)
    <+> (cVertical .^ v)
    <-> cOrigin
    )
  where
    theta      = fov * pi / 180
    halfHeight = tan $ theta / 2
    halfWidth  = aspect * halfHeight
    w          = normalize $ from <-> at
    u          = normalize $ vup >< w
    v          = w >< u
    cLowerLeftCorner = from
      <-> (u .^ (halfWidth * focusDist))
      <-> (v .^ (halfHeight * focusDist))
      <-> (w .^ focusDist)
    cHorizontal      = u .^ (2 * halfWidth * focusDist)
    cVertical        = v .^ (2 * halfHeight * focusDist)
    cOrigin          = from


{-


getRayNormPersp :: Camera -> Double -> Double -> Ray
getRayNormPersp c u v = Ray
  (cOrigin c)
  (   cLowerLeftCorner c
  <+> ((cHorizontal c) .^ u)
  <+> ((cVertical c) .^ v)
  <-> (cOrigin c)
  )

getCameraReflectedThroughUV c =
  let
    uvC = (   cLowerLeftCorner c
          <+> ((cHorizontal c) .^ 0.5)
          <+> ((cVertical c) .^ 0.5)
          -- <-> (cOrigin c)
          )
    (CVec3 x y' z) = uvC .^ 2 <-> (cOrigin c)
    (CVec3 _ y _) = cOrigin c
  in CVec3 x y z


getRayRevPersp :: Camera -> Double -> Double -> Ray
getRayRevPersp c u v = Ray
  (   cLowerLeftCorner c
  <+> ((cHorizontal c) .^ u)
  <+> ((cVertical c) .^ v))
  (cRP <-> (   cLowerLeftCorner c
  <+> ((cHorizontal c) .^ u)
  <+> ((cVertical c) .^ v)
  ))
  where
    cRP = getCameraReflectedThroughUV c

-- getRay = getRayRevPersp
-- getRay = getRayNormPersp
-}
