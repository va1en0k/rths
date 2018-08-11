module World where

import Data.Vec3

import Hitable
import Vectors
import Material

objects :: [Hitable_]
objects = [
  MkHitable $ Sphere (mkLambertian $ CVec3 0.8 0.3 0.3) (CVec3 0 0 (-1)) 0.5,
  MkHitable $ Sphere (mkLambertian $ CVec3 0.8 0.8 0.0) (CVec3 0 (-100.5) (-1)) 100,
  MkHitable $ Sphere (mkMetal 0.3 $ CVec3 0.8 0.6 0.2) (CVec3 1 0 (-1)) 0.5,
  MkHitable $ Sphere (mkMetal 1.0 $ CVec3 0.8 0.8 0.8) (CVec3 (-1) 0 (-1)) 0.5
  ]


data Sphere = Sphere Material CVec3 Double

instance Hitable Sphere where
  hit (Sphere m sc sr) r@(Ray org dir) mn mx =
    let oc = org <-> sc
        a = dir .* dir
        b = 2 * oc .* dir
        c = (oc .* oc) - (sr * sr)
        dsc = b * b - 4 * a * c
        hf x = Just h
          where
            p = rayPointAt r x
            -- n = normalize $ p <-> CVec3 0 0 (-1)
            n = mapv (/ sr) (p <-> sc)
            h = Hit x p n (scatterF m r h)
    in if dsc < 0 then Nothing
       else let x0 = ( - b - sqrt dsc ) / (2 * a)
                x1 = ( - b + sqrt dsc ) / (2 * a)
            in if x0 >= mn && x0 <= mx then hf x0
               else if x1 >= mn && x1 <= mx then hf x1
                 else Nothing
