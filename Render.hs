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
import Hitable
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

camera = Camera (CVec3 0 0 0) (CVec3 3 0 0) (CVec3 0 (-3) 0) (CVec3 (-2) 2 (-1))

type ImgBuf = Array (Int, Int) PixelRGB8

renderUV :: RandomGen g => Double -> Double -> Rand g Color
-- renderUV u v = CVec3 u v 0.2
renderUV u v = traceColor objects $ Ray (cOrigin camera) (cLowerLeftCorner camera <+> (u *. (cHorizontal camera)) <+> (v *. (cVertical camera)))


colorToPixel :: Color -> PixelRGB8
colorToPixel c =
  let (r, g, b) = toXYZ $ mapv ((* 255.9) . sqrt) c
  in PixelRGB8 (fromInteger $ floor r) (fromInteger $ floor g) (fromInteger $ floor b)

renderOnce :: RandomGen g => Int -> Int -> Rand g Color
renderOnce x y =
  do (a:b:_) <- getRandomRs (0::Double, 1)
     renderUV ((a + fromIntegral x) / kRes) ((b + fromIntegral y) / kRes)

-- render :: RandomGen g => Int -> Int -> Rand g PixelRGB8
-- render :: Int -> [Hitable_] -> Int -> Int -> IO PixelRGB8
render :: Int -> Int -> IO PixelRGB8
render x y = colorToPixel <$> avgv <$> rendersIO
  where
    gens = replicateM 20 newStdGen
    rendersIO :: IO [Color]

    rendersIO = do
      gs <- gens
      let rs = map (runIdentity . evalRandT (renderOnce x y)) gs
      -- when (x == 200 && y == 40) (print rs)
      return $ rs --rs `deepseq` rs




genImageBuf :: Int -> Int -> IO ImgBuf
genImageBuf w h = array ((0, 0), (w, h)) <$> lsIO
  where
    -- ls :: RandT g Identity [((Int, Int), PixelRGB8)]
    allPixels = [(i, j) | i <- [0..w], j <- [0..h]]

    ls = mapWithProgressBar (uncurry $ render) allPixels

    lsIO :: IO [((Int, Int), PixelRGB8)]
    lsIO = zip allPixels <$> ls

genImageF :: Int -> Int -> IO (Int -> Int -> PixelRGB8)
genImageF w h = f <$> genImageBuf w h
  where
    -- f b x y | trace ((show x) ++ " " ++ (show y)) False = undefined
    f b x y = b ! (x, y)

-- renderAntialiased :: Int -> Int -> PixelRGB8
-- renderAntialiased x y = colorToPixel $ (1 / (fromIntegral $ length points)) *. vsum points
--   where
--     points =
--       do i <- [0.1..0.9]
--          j <- [0.1..0.9]
--          return $ renderUV ((i + fromIntegral x) / kRes) ((j + fromIntegral y) / kRes)
