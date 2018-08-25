{-# LANGUAGE QuasiQuotes #-}

module Parallel.OpenCLGeometry where

import Data.String.Interpolate
-- import           Language.C.Quote.OpenCL         (cfun)

import Data.Vec3

import Types
import World
import Parallel.Shaders hiding (main)

sphereSource :: String
sphereSource = [i|
    struct ray
    {
        float3 origin;
        float3 direction;
    };

    struct hit
    {
        float t;
        float3 point;
        float3 normal;
    };

    bool sphereHit_1(struct ray r, float from, float to, struct hit * hit);

    kernel void doubleArray(
        global float8 *frayIn,
        global float8 *fout
    ) {

        int i = get_global_id(0);

        struct ray rayIn = {
          {frayIn[i][0], frayIn[i][1], frayIn[i][2]},
          {frayIn[i][3], frayIn[i][4], frayIn[i][5]}
        };

        struct hit h;
        if (!sphereHit_1(rayIn, -10000, 100000, &h)) {
          h.t = -22;
        }

        /*struct hit h = {
          rayIn.direction.x + rayIn.direction.y/10,
          {rayIn.direction.y + rayIn.direction.z/10,4,5},
          {1,2,3}
        };*/

        fout[i] = (float8)(
          h.t,
          h.point[0], h.point[1], h.point[2],
          h.normal[0], h.normal[1], h.normal[2],
          -1
        );
    }
|]

v (CVec3 x y z) = [i|(float3)(#{x}, #{y}, #{z})|]

compileSphere id (Sphere _ center radius) = [i|

bool sphereHit_#{id}(struct ray r, float from, float to, struct hit * hit) {
  float3 oc = r.origin - #{v center};
  float a = dot(r.direction, r.direction);
  float b = 2.0 * dot(oc, r.direction);
  float c = dot(oc, oc) - #{radius * radius};
  float dsc = b * b - 4.0 * a * c;

  if (dsc < 0.0) {
    hit->t = dsc;
    hit->point = oc;
    hit->normal = r.origin;
    return false;
  }
  float sqDsc = sqrt(dsc);
  float x = - b - sqDsc / 2.0 * a;
  if (!(x >= from && x <= to)) {
    x = - b - sqDsc / 2.0 * a;
  }
  /*if (!(x >= from && x <= to)) {
    return false;
  }*/
  hit->t = x;
  hit->point = r.origin + x * r.direction;
  hit->normal = (hit->point - #{v center}) / #{radius}f;

  return true;
}

|]

rayToDoubles :: Ray -> [Double]
rayToDoubles (Ray (CVec3 a b c) (CVec3 d e f)) = [a, b, c, d, e, f, -1, -1]

doublesToHit :: [Double] -> Hit
doublesToHit [t,a,b,c,x,y,z,-1] = Hit t (CVec3 a b c) (CVec3 x y z) undefined

takeBy cnt [] = []
takeBy cnt xs = (take cnt xs : takeBy cnt (drop cnt xs))

-- runGeometryShader :: Shader -> [Ray] -> [Hit]
runGeometryShader :: ShaderEngine -> Shader -> [Ray] -> IO [Hit]
runGeometryShader e s rs = do
  let rcnt = length rs
  output <- runOnShader e s (rcnt * 8) [(concatMap rayToDoubles rs)]
  return $ map doublesToHit $ takeBy 8 output

main :: IO ()
main = do
  -- print $ compileSphere $ Sphere undefined (CVec3 0 0 (-1)) 0.5
  engine <- initShaderEngine
  let s = sphereSource ++ compileSphere "1" (Sphere undefined (CVec3 0 0 0) 2)
  putStrLn s
  shader <- createShader engine s
  output <- runGeometryShader engine shader [Ray (CVec3 0 0 0) (CVec3 0 1 0), Ray (CVec3 7 8 9) (CVec3 10 11 12)]
  putStrLn $ "Output: " ++ show output
