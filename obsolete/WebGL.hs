{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

-- geometryToWebgl :: Geometry -> String

module WebGL where

import Data.Text (Text)
import Data.String.Interpolate
import Data.Vec3


import Types
import World


-- types :: Text
types = [i|
struct Ray {vec3 origin; vec3 direction;};
struct Hit {float t; vec3 point; vec3 normal;};
|]
-- --
-- instance Show CVec3 where
--   show (CVec3 x y z) = "hi"

-- p :: a -> Text
v (CVec3 x y z) = [i|vec3(#{x}, #{y}, #{z})|]

-- vec2(u, v) -> [ray] (vec3, vec3)
-- cameraToWebgl :: Camera -> Text
cameraToWebgl (Camera cOrigin cHorizontal cVertical cLowerLeftCorner) = [i|
Ray cam(vec2 uv) {
  return Ray(
    #{v cOrigin},
    #{v $ cLowerLeftCorner <-> cOrigin } + uv.x * #{ v cHorizontal } + uv.y * #{ v cVertical }
  );
}
|]
--
--
-- instance Hitable Sphere where
--   hit (Sphere m sc sr) r@(Ray org dir) mn mx =
--     let oc = org <-> sc
--         a = dir .* dir
--         b = 2 * oc .* dir
--         c = (oc .* oc) - (sr * sr)
--         dsc = b * b - 4 * a * c
--         hf x = Just h
--           where
--             p = rayPointAt r x
--             -- n = normalize $ p <-> CVec3 0 0 (-1)
--             n = mapv (/ sr) (p <-> sc)
--             h = Hit x p n (scatterF m r h)
--     in if dsc < 0 then Nothing
--        else let x0 = ( - b - sqrt dsc ) / (2 * a)
--                 x1 = ( - b + sqrt dsc ) / (2 * a)
--             in if x0 >= mn && x0 <= mx then hf x0
--                else if x1 >= mn && x1 <= mx then hf x1
--                  else Nothing

-- geometryToWebgl a =
sphereToWebgl id (Sphere material center radius) = [i|
bool sphereHit_#{id}(Ray r, float from, float to, out Hit hit) {
  vec3 oc = r.origin - #{v center};
  float a = dot(r.direction, r.direction);
  float b = 2.0 * dot(oc, r.direction);
  float c = dot(oc, oc) - #{radius * radius};
  float dsc = b * b - 4.0 * a * c;

  if (dsc < 0.0) return false;
  float sqDsc = sqrt(dsc);
  float x = - b - sqDsc / 2.0 * a;
  if (!(x >= from && x <= to)) {
    x = - b - sqDsc / 2.0 * a;
  }
  if (!(x >= from && x <= to)) {
    return false;
  }
  hit.t = x;
  hit.point = r.origin + x * r.direction;
  hit.normal = (hit.point - #{v center}) / #{radius};
}

}
|]
