module Parallel.Render where

import Codec.Picture
import Data.Vec3
import Data.Maybe
import Data.List
import Data.Array
import Data.Function
import Control.Monad.Random
import Control.Monad.Primitive
import Control.Monad.Identity
import Debug.Trace
import Data.Time.Clock.POSIX
import System.Environment


-- import Normal

import Vectors
import Types
import Progress
import World
import RayTracer

import RTMonad

import Render (res, kRes, camera, getRay, ImgBuf, colorToPixel)

import Parallel.OpenCLGeometry
import Parallel.Shaders
--
-- renderUV :: RandomGen g => World -> Double -> Double -> Rand g Color
-- -- renderUV u v = CVec3 u v 0.2
-- renderUV world u v = traceColor world $ getRay camera  u v
--
-- renderOnce :: RandomGen g => World -> Int -> Int -> Rand g Color
-- renderOnce world x y =
--   do (a:b:_) <- getRandomRs (0::Double, 1)
--      renderUV world ((a + fromIntegral x) / (fromIntegral $ fst res)) ((b + fromIntegral y) / (fromIntegral $ snd res))
--
-- -- render :: RandomGen g => Int -> Int -> Rand g PixelRGB8
-- -- render :: Int -> [Hitable_] -> Int -> Int -> IO PixelRGB8
-- render :: World -> Int -> Int -> IO PixelRGB8
-- render world x y = colorToPixel <$> avgv <$> rendersIO
--   where
--     gens = replicateM genCount newStdGen
--     rendersIO :: IO [Color]
--
--     rendersIO = do
--       gs <- gens
--       let rs = map (runIdentity . evalRandT (renderOnce world x y)) gs
--       -- when (x == 200 && y == 40) (print rs)
--       return $ rs --rs `deepseq` rs
--
--

toUV :: (Int, Int) -> (Double, Double)
toUV (x, y) = (((fromIntegral x) / (fromIntegral $ fst res)), ((fromIntegral y) / (fromIntegral $ snd res)))


prepareShader :: RT ()
prepareShader = runIO (do
  engine <- initShaderEngine
  shader <- createShader engine sphereSource
  return (engine, shader)) >>= \(engine, shader) -> updateSettings (\s -> s {shaderEngine = engine, rayTraceShader = shader})


hits :: [Ray] -> RT [Maybe Hit]
hits rs = do
  (Settings w e s) <- getSettings
  runIO $ runGeometryShader e s (map asSphere w) rs

-- bulkmapOnlyExisting :: [Maybe a] -> ([a] -> [b]) -> [Maybe b]
-- bulkmapOnlyExisting ms f = if length xs == 0 then map (const Nothing) ms else replaceJusts ms (f xs)
--   where
--     xs = catMaybes ms
--     replaceJusts [] [] = []
--     replaceJusts (Nothing:ms) rs = Nothing:replaceJusts ms rs
--     replaceJusts (Just _:ms) (r:rs) = replaceJusts ms rs

randomInUnitBall = return (CVec3 0.1 0.2 0.3)

nextRay :: Hit -> RT Ray
nextRay (Hit {hitNormal=n, hitP=p}) = Ray p <$> ((n <+>) <$> randomInUnitBall)

colors :: Int -> [Ray] -> RT [Color]
colors 5 rs = return $ map sky rs
colors i rs = do
  hs <- hits rs :: RT [Maybe Hit]
  nextRays <- mapM nextRay $ catMaybes hs :: RT [Ray]
  cs <- colors (i+1) nextRays :: RT [Color]
  return $ applyColors rs hs cs
  where
    applyColors :: [Ray] -> [Maybe Hit] -> [Color] -> [Color]
    applyColors [] [] [] = []
    applyColors (r:rs) (Nothing:hs) cs = sky r : applyColors rs hs cs
    applyColors (r:rs) (Just h:hs) (c:cs) = mapv (/2) c : applyColors rs hs cs

  -- bulkmapOnlyExisting processFurther hs
  -- processFurther :: [Hit] -> [Color]

genImageBuf :: Int -> Int -> RT ImgBuf
genImageBuf w h = array ((0, 0), (w, h)) <$> lsRT
  where
    -- ls :: RandT g Identity [((Int, Int), PixelRGB8)]
    allPixels = [(i, j) | i <- [0..w], j <- [0..h]]

    allRays = map ((uncurry $ getRay camera) . toUV) allPixels

    lsRT = do
      -- hs <- hits allRays
      -- runIO $ print hits
      -- let ps = map (colorToPixel . (mapv (/10)) . hitNormal . fromMaybe (Hit undefined undefined (CVec3 0 0 0) undefined)) hs
      -- let
      --   redIfHit (Just _) = CVec3 0.9 0.1 0.3
      --   redIfHit Nothing = CVec3 0 0 0
      -- let ps = map (colorToPixel . redIfHit) hits
      ps <- map colorToPixel <$> colors 0 allRays
      return $ zip allPixels ps
    -- ls = mapWithProgressBar (uncurry $ renderPixelOnShader world) allPixels
    -- ls = return $ map (const $ colorToPixel (CVec3 200 100 100)) allPixels

    -- lsIO :: RT [((Int, Int), PixelRGB8)]
    -- lsIO = zip allPixels <$> ls


genImageF :: Int -> Int -> RT (Int -> Int -> PixelRGB8)
genImageF w h = prepareShader >> (f <$> genImageBuf w h)
  where
    -- f b x y | trace ((show x) ++ " " ++ (show y)) False = undefined
    f b x y = b ! (x, y)
