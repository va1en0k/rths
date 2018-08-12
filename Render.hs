module Render where

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

res :: (Int, Int)
res = (300, 200)
kRes :: Double
kRes = fromIntegral $ uncurry min $ res

data Camera = Camera {
  cOrigin :: CVec3,
  cHorizontal :: CVec3,
  cVertical :: CVec3,
  cLowerLeftCorner :: CVec3
}

-- camera = Camera (CVec3 0 0 0) (CVec3 2.5 0 0) (CVec3 0 (-2.5) 0) (CVec3 (-2) 1 (-1))

-- camera = mkCamera 90 (fromIntegral (fst res) / fromIntegral (snd res))

camera = mkCamera (CVec3 13 2 3) (CVec3 0 0 0) (CVec3 0 (-1) 0) 20 (fromIntegral (fst res) / fromIntegral (snd res)) 10

mkCamera :: CVec3 -> CVec3 -> CVec3 -> Double -> Double -> Double -> Camera
mkCamera from at vup vfov aspect focusDist =
  Camera {
    cLowerLeftCorner = from <-> ((halfWidth * focusDist) *. u) <-> ((halfHeight * focusDist) *. v) <-> (focusDist *. w),
    cHorizontal = (2 * halfWidth * focusDist) *. u,
    cVertical = (2 * halfWidth * focusDist) *. v,
    cOrigin = from
  }
    where theta = vfov * pi / 180
          halfHeight = tan $ theta / 2
          halfWidth = aspect * halfHeight
          w = normalize $ from <-> at
          u = normalize $ vup >< w
          v = w >< u

getRay :: Camera -> Double -> Double -> Ray
getRay c u v = Ray (cOrigin c) (cLowerLeftCorner c <+> (u *. (cHorizontal c)) <+> (v *. (cVertical c)) <-> (cOrigin c))

type ImgBuf = Array (Int, Int) PixelRGB8

renderUV :: RandomGen g => World -> Double -> Double -> Rand g Color
-- renderUV u v = CVec3 u v 0.2
renderUV world u v = traceColor world $ getRay camera  u v


colorToPixel :: Color -> PixelRGB8
colorToPixel c =
  let (r, g, b) = toXYZ $ mapv ((* 255.9) {-. sqrt-}) c
  in PixelRGB8 (fromInteger $ floor r) (fromInteger $ floor g) (fromInteger $ floor b)

renderOnce :: RandomGen g => World -> Int -> Int -> Rand g Color
renderOnce world x y =
  do (a:b:_) <- getRandomRs (0::Double, 1)
     renderUV world ((a + fromIntegral x) / (fromIntegral $ fst res)) ((b + fromIntegral y) / (fromIntegral $ snd res))

-- render :: RandomGen g => Int -> Int -> Rand g PixelRGB8
-- render :: Int -> [Hitable_] -> Int -> Int -> IO PixelRGB8
render :: World -> Int -> Int -> IO PixelRGB8
render world x y = colorToPixel <$> avgv <$> rendersIO
  where
    gens = replicateM 20 newStdGen
    rendersIO :: IO [Color]

    rendersIO = do
      gs <- gens
      let rs = map (runIdentity . evalRandT (renderOnce world x y)) gs
      -- when (x == 200 && y == 40) (print rs)
      return $ rs --rs `deepseq` rs

genImageBuf :: World -> Int -> Int -> IO ImgBuf
genImageBuf world w h = array ((0, 0), (w, h)) <$> lsIO
  where
    -- ls :: RandT g Identity [((Int, Int), PixelRGB8)]
    allPixels = [(i, j) | i <- [0..w], j <- [0..h]]

    ls = mapWithProgressBar (uncurry $ render world) allPixels

    lsIO :: IO [((Int, Int), PixelRGB8)]
    lsIO = zip allPixels <$> ls

genImageF :: World -> Int -> Int -> IO (Int -> Int -> PixelRGB8)
genImageF world w h = f <$> genImageBuf world w h
  where
    -- f b x y | trace ((show x) ++ " " ++ (show y)) False = undefined
    f b x y = b ! (x, y)
