module World where

import Data.Vec3

import Hitable
import Vectors


objects :: [Hitable_]
objects = [
  MkHitable $ Sphere (CVec3 0.7 0 (-1)) 0.5,
  -- MkHitable $ Sphere (CVec3 (-1) (-0.3) (-0.8)) 0.2,
  MkHitable $ Sphere (CVec3 0 (-100.5) (-1)) 100 ]


data Sphere = Sphere CVec3 Double

instance Hitable Sphere where
  hit (Sphere sc sr) r@(Ray org dir) mn mx =
    let oc = org <-> sc
        a = dir .* dir
        b = 2 * oc .* dir
        c = (oc .* oc) - (sr * sr)
        dsc = b * b - 4 * a * c
        hf x = Just $ Hit x p n
          where
            p = rayPointAt r x
            -- n = normalize $ p <-> CVec3 0 0 (-1)
            n = mapv (/ sr) (p <-> sc)
    in if dsc < 0 then Nothing
       else let x0 = ( - b - sqrt dsc ) / (2 * a)
                x1 = ( - b + sqrt dsc ) / (2 * a)
            in if x0 >= mn && x0 <= mx then hf x0
               else if x1 >= mn && x1 <= mx then hf x1
                 else Nothing
