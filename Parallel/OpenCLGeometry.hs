{-# LANGUAGE QuasiQuotes #-}

module Parallel.OpenCLGeometry where

import Data.String.Interpolate
-- import           Language.C.Quote.OpenCL         (cfun)

import Data.Vec3
import Data.Maybe
import Data.List
import Data.Ord

import Types
import World
import RayTracer
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

    struct sphere
    {
        float3 center;
        float radius;
    };


    bool sphereHit(struct sphere s, struct ray r, float from, float to, struct hit * hit) {
      float3 oc = r.origin - s.center;
      float a = dot(r.direction, r.direction);
      float b = 2.0 * dot(oc, r.direction);
      float c = dot(oc, oc) - s.radius * s.radius;
      float dsc = b * b - 4.0 * a * c;

      /*hit->t = s.radius;
      hit->point = s.center;
      hit->normal = r.direction;
      return true;*/

      if (dsc < 0.0) {
        hit->t = 0;
        hit->point = (float3)(0, 0, 0);
        hit->normal = (float3)(0, 0, 0);
        return false;
      }
      float sqDsc = sqrt(dsc);
      float x = (- b - sqDsc) / (2.0 * a);
      if (!(x >= from && x <= to)) {
        x = (- b + sqDsc) / (2.0 * a);
      }
      if (!(x >= from && x <= to)) {
        return false;
      }
      hit->t = x;
      hit->point = r.origin + x * r.direction;
      hit->normal = (hit->point - s.center) / s.radius;

      return true;
    }

    kernel void doubleArray(
        global float8 *frayIn,
        global float4 *allSpheres,
        global float8 *fout
    ) {

        int i = get_global_id(0);
        int sphereId = get_global_id(1);
        int outId = get_global_size(1) * i + sphereId;
/*        fout[outId] = (float8)(
          i,
          sphereId,
          get_global_size(0),get_global_size(1),-1,-1,-1,-1
        );
        return;*/

        struct ray rayIn = {
          {frayIn[i][0], frayIn[i][1], frayIn[i][2]},
          {frayIn[i][3], frayIn[i][4], frayIn[i][5]}
        };
        struct sphere sphere = {
          {allSpheres[sphereId][0], allSpheres[sphereId][1], allSpheres[sphereId][2]},
          allSpheres[sphereId][3]
        };

        //struct sphere s = {{0.1, 0.1, 0.1}, 1.1};

        struct hit h;
        if (!sphereHit(sphere, rayIn, 0, 100000, &h)) {
          h.t = -1;
        }

        /*struct hit h = {
          rayIn.direction.x + rayIn.direction.y/10,
          {rayIn.direction.y + rayIn.direction.z/10,4,5},
          {1,2,3}
        };*/

        fout[outId] = (float8)(
          h.t,
          h.point[0], h.point[1], h.point[2],
          h.normal[0], h.normal[1], h.normal[2],
          -1
        );
    }
|]

-- v (CVec3 x y z) = [i|(float3)(#{x}, #{y}, #{z})|]

rayToDoubles :: Ray -> [Double]
rayToDoubles (Ray (CVec3 a b c) (CVec3 d e f)) = [a, b, c, d, e, f, -1, -1]

sphereToDoubles :: Sphere -> [Double]
sphereToDoubles (Sphere _ (CVec3 a b c) r) = [a, b, c, r]

doublesToHit :: [Double] -> Hit
doublesToHit [t,a,b,c,x,y,z,-1] = Hit t (CVec3 a b c) (CVec3 x y z) undefined

takeBy cnt [] = []
takeBy cnt xs = (take cnt xs : takeBy cnt (drop cnt xs))

-- runGeometryShader :: Shader -> [Ray] -> [Hit]
runGeometryShader :: ShaderEngine -> Shader -> [Sphere] -> [Ray] -> IO [Maybe Hit]
runGeometryShader e s spheres rs = do
  let rcnt = length rs
  -- print (rcnt * 2)
  output <- runOnShader e s rcnt 8 [(map rayToDoubles rs), (map sphereToDoubles spheres)]
  -- print $ takeBy 8 output
  let hits = map doublesToHit $ takeBy 8 output
  let hitsByRay = takeBy 1 hits
  let goodRaysF = catMaybes . map (\h -> if hitT h <= 0 then Nothing else Just h)
  let validHitsByRay = map goodRaysF hitsByRay
  let bestHitByRay = map (\xs -> if length xs == 0 then Nothing else Just (minimumBy (comparing hitT) xs)) validHitsByRay
  return bestHitByRay


worldOfSpheres = map asSphere objects
-- worldOfSpheres = [Sphere undefined (CVec3 2 0 0) 0.9]

testRays = [Ray (CVec3 0 0 0) (CVec3 0 1 0),
              Ray (CVec3 7 8 9) (CVec3 10 11 12),
              Ray (CVec3 0 0 0) (CVec3 0 1 0)]

-- testRays = [Ray (CVec3 0 0 0) (CVec3 1 0 0)]

main :: IO ()
main = do
  -- print $ compileSphere $ Sphere undefined (CVec3 0 0 (-1)) 0.5
  engine <- initShaderEngine
  -- let s = sphereSource
  -- putStrLn s
  -- print worldOfSpheres
  shader <- createShader engine sphereSource
  output <- runGeometryShader engine shader worldOfSpheres testRays
  putStrLn $ "Output shaders: " ++ show output
  -- print $ map (\r -> hit (head worldOfSpheres) r 0.00001 maxFloat) testRays
  print $ map (\r -> hit objects r 0.00001 maxFloat) testRays
