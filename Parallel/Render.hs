module Parallel.Render where

import           Codec.Picture
import           Data.Vec3
import           Data.Maybe
import           Data.List
import           Data.Array
import           Data.Either
import           Data.Function
import           Control.Monad.Random
import           Control.Monad.Primitive
import           Control.Monad.Identity
import           Debug.Trace
import           Data.Time.Clock.POSIX
import           System.Environment


-- import Normal

import           Geometry.Vectors
import           Types
-- import Progress
import           World
-- import RayTracer

import           RTMonad
import           Random
import           Util
import           Config

-- import Render (kRes, camera, getRay, ImgBuf, colorToPixel)

import           Parallel.OpenCLGeometry
import           Parallel.Shaders

import           Graphics.Images

import           Geometry.Camera


sky :: Ray -> Color
sky r = ((1.0 - t) *. CVec3 1 1 1) <+> (t *. CVec3 0.5 0.7 1.0)
 where
  CVec3 _ y _ = normalize $ direction r
  t           = (y + 1) * 0.5


toUV :: (Int, Int) -> (Double, Double)
toUV (x, y) =
  ( ((fromIntegral x) / (fromIntegral $ fst res))
  , ((fromIntegral y) / (fromIntegral $ snd res))
  )


prepareShader :: RT ()
prepareShader =
  runIO
      (do
        engine <- initShaderEngine
        source <- fullSource
        shader <- createShader engine source
        return (engine, shader)
      )
    >>= \(engine, shader) -> updateSettings
          (\s -> s { shaderEngine = engine, rayTraceShader = shader })


-- hits :: [Ray] -> RT [Maybe Hit]
-- hits rs = do
--   (Settings w e s) <- getSettings
--   runIO $ runGeometryShader e s (map asSphere w) rs

hits :: [Ray] -> RT [Maybe Hit]
hits rs = do
  (Settings world e s) <- getSettings
  -- runIO $ runGeometryShader e s (map asSphere world) rs
  runIO $ print (length rs)
  return $ map (\r -> hit world r 0.0001 1000) rs


nextRay :: Hit -> RT Ray
nextRay (Hit { hitNormal = n, hitP = p }) =
  Ray p <$> ((n <+>) <$> randomInUnitBall'')

-- applyMaterial :: Hit -> Color -> Color
-- applyMaterial (Hit {hitSphere=(Sphere m c r)}) nextColor =
--   mapv ((/16) . (+8) . abs) c <+> mapv (/2) nextColor

applyMaterial :: Ray -> Hit -> RT (Either Color (Color, Ray))
applyMaterial r h =
  (scatterF $ hitMaterial h) r h >>= \sc -> return $ case sc of
    Just (c, r) -> Right (c, r)
    Nothing     -> Left (CVec3 0 0 0)

-- scatterF :: Ray -> Hit -> RT (Color, Ray)

divideAndConquer :: ([a] -> RT [c]) -> ([b] -> RT [c]) -> [Either a b] -> RT [c]
divideAndConquer lf rf eithers = do
  let (ls, rs) = partitionEithers eithers

  lr <- lf ls
  rr <- rf rs

  let recollect []             []       []       = []
      recollect (Left  _ : es) (l : lr) rr       = l : recollect es lr rr
      recollect (Right _ : es) lr       (r : rr) = r : recollect es lr rr

  return $ recollect eithers lr rr


colors :: Int -> [Ray] -> RT [Color]
colors 32 rs = return $ map sky rs
colors i   rs = do
  hs <- hits rs :: RT [Maybe Hit]

  let asResult (ray, Just hit) = Right (ray, hit)
      asResult (ray, Nothing ) = Left ray

      applyScatters :: [(Color, Ray)] -> RT [Color]
      applyScatters crs = do
        nextColors <- colors (i + 1) (map snd crs)
        return $ map (uncurry (*<>*)) $ zip (map fst crs) nextColors

      -- applyMaterials :: [(Ray, Hit)] -> Either Color (Color, Ray)
      -- applyMaterials =

      actions = map asResult $ zip rs hs :: [Either Ray (Ray, Hit)]

  nextActions <-
    divideAndConquer (return . map (Left . sky))
                     (mapM (uncurry applyMaterial))
                     actions :: RT [Either Color (Color, Ray)]

  colors <- divideAndConquer return applyScatters nextActions :: RT [Color]

  return colors
  --
  -- let matches = map (uncurry z) $ zip rs hs :: RT [Maybe (Ray, Hit)]
  -- scatters <- mapM applyMaterial matches :: RT [Maybe (Color, Ray)]
  --
  -- nextRays <- mapM nextRay $ catMaybes hs :: RT [Ray]
  --
  -- cs <- if length nextRays > 0 then colors (i+1) nextRays else return [] :: RT [Color]
  -- return $ applyColors rs hs cs
  -- where
  --
  --   applyColors :: [Ray] -> [Maybe Hit] -> [Color] -> [Color]
  --   applyColors [] [] [] = []
  --   applyColors (r:rs) (Nothing:hs) cs = sky r : applyColors rs hs cs
  --   applyColors (r:rs) (Just h:hs) (c:cs) = applyMaterial h c : applyColors rs hs cs

  -- bulkmapOnlyExisting processFurther hs
  -- processFurther :: [Hit] -> [Color]

genImageBuf :: Int -> Int -> RT ImgBuf
genImageBuf w h = array ((0, 0), (w, h)) <$> lsRT
 where
    -- ls :: RandT g Identity [((Int, Int), PixelRGB8)]
  allPixels = [ (i, j) | i <- [0 .. w], j <- [0 .. h] ]

  allUVs    = map toUV allPixels
  allUVsAA  = concat <$> mapM uvsAA allPixels

  uvsAA :: (Int, Int) -> RT [(Double, Double)]
  uvsAA (x, y) = mapM (const $ uvAA (x, y)) [1 .. aaGenCount]

  uvAA :: (Int, Int) -> RT (Double, Double)
  uvAA (x, y) = do
    (a : b : _) <- getRands
    return
      $ ( ((a + fromIntegral x) / (fromIntegral $ fst res))
        , ((b + fromIntegral y) / (fromIntegral $ snd res))
        )

  lsRT = do
    allRaysAA <- map (uncurry $ getRay camera) <$> allUVsAA
    ps        <-
      map colorToPixel <$> map avgv <$> takeBy aaGenCount <$> colors 0 allRaysAA
    return $ zip allPixels ps


genImageF :: Int -> Int -> RT (Int -> Int -> PixelRGB8)
genImageF w h = prepareShader >> (f <$> genImageBuf w h)
  where
    -- f b x y | trace ((show x) ++ " " ++ (show y)) False = undefined
        f b x y = b ! (x, y)
