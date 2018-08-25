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
genImageBuf :: Int -> Int -> RT ImgBuf
genImageBuf w h = array ((0, 0), (w, h)) <$> lsIO
  where
    -- ls :: RandT g Identity [((Int, Int), PixelRGB8)]
    allPixels = [(i, j) | i <- [0..w], j <- [0..h]]

    -- ls = mapWithProgressBar (uncurry $ renderPixelOnShader world) allPixels
    ls = return $ map (const $ colorToPixel (CVec3 200 100 100)) allPixels

    lsIO :: RT [((Int, Int), PixelRGB8)]
    lsIO = zip allPixels <$> ls

genImageF :: Int -> Int -> RT (Int -> Int -> PixelRGB8)
genImageF w h = f <$> genImageBuf w h
  where
    -- f b x y | trace ((show x) ++ " " ++ (show y)) False = undefined
    f b x y = b ! (x, y)