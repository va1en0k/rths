{-# LANGUAGE QuasiQuotes #-}

module Parallel.OpenCLGeometry where

import           Data.String.Interpolate
-- import           Language.C.Quote.OpenCL         (cfun)

import           Data.Vec3
import           Data.Maybe
import           Data.List
import           Data.Ord

import           Types
import           World
-- import RayTracer
import           Parallel.Shaders        hiding ( main )
import           Util

fullSource :: IO String
fullSource = readFile "Parallel/sphere.c"

-- v (CVec3 x y z) = [i|(float3)(#{x}, #{y}, #{z})|]

rayToDoubles :: Ray -> [Double]
rayToDoubles (Ray (CVec3 a b c) (CVec3 d e f)) = [a, b, c, d, e, f, -1, -1]

sphereToDoubles :: Sphere -> [Double]
sphereToDoubles (Sphere _ (CVec3 a b c) r) = [a, b, c, r]

doublesToHit :: [Sphere] -> [Double] -> Hit
doublesToHit spheres [t, a, b, c, x, y, z, sid] =
  Hit t (CVec3 a b c) (CVec3 x y z) (spheres !! floor sid)



-- runGeometryShader :: Shader -> [Ray] -> [Hit]
runGeometryShader
  :: ShaderEngine -> Shader -> [Sphere] -> [Ray] -> IO [Maybe Hit]
runGeometryShader e s spheres rs = do
  let rcnt = length rs
  -- print (rcnt * 2)
  output <- runOnShader e
                        s
                        rcnt
                        8
                        [(map rayToDoubles rs), (map sphereToDoubles spheres)]
  -- print $ takeBy 8 output
  let hits = map (doublesToHit spheres) $ takeBy 8 output
  let hitsByRay = takeBy 1 hits
  let goodRaysF =
        catMaybes . map (\h -> if hitT h <= 0 then Nothing else Just h)
  let validHitsByRay = map goodRaysF hitsByRay
  let
    bestHitByRay = map
      (\xs -> if length xs == 0
        then Nothing
        else Just (minimumBy (comparing hitT) xs)
      )
      validHitsByRay
  return bestHitByRay


worldOfSpheres = map asSphere objects
-- worldOfSpheres = [Sphere undefined (CVec3 2 0 0) 0.9]

testRays =
  [ Ray (CVec3 0 0 0) (CVec3 0 1 0)
  , Ray (CVec3 7 8 9) (CVec3 10 11 12)
  , Ray (CVec3 0 0 0) (CVec3 0 1 0)
  ]

-- testRays = [Ray (CVec3 0 0 0) (CVec3 1 0 0)]

main :: IO ()
main = do
  -- print $ compileSphere $ Sphere undefined (CVec3 0 0 (-1)) 0.5
  engine <- initShaderEngine
  -- let s = fullSource
  -- putStrLn s
  -- print worldOfSpheres
  source <- fullSource
  shader <- createShader engine source
  output <- runGeometryShader engine shader worldOfSpheres testRays
  putStrLn $ "Output shaders: " ++ show output
  -- print $ map (\r -> hit (head worldOfSpheres) r 0.00001 maxFloat) testRays
  print $ map (\r -> hit objects r 0.00001 maxFloat) testRays
