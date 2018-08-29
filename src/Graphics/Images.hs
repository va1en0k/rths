module Graphics.Images (
  Image(..),
  PixelRGB8(..),
  ImgBuf,
  colorToPixel,
  writePng, readPng,
  superimpose
  ) where

import qualified Codec.Picture as P
import           Codec.Picture hiding (Image, writePng, readPng)
import           Data.Array
import           Linear.V3
import           Linear.Metric

import           Geometry.Vectors
import           Types
import           Util

type ImgBuf = Array (Int, Int) PixelRGB8

data Image = Image (Int, Int) ((Int, Int) -> PixelRGB8)

superimpose :: Image -> Image -> Image
superimpose (Image (w, h) f1) (Image (w1, h1) f2) = Image (w, h) imF
  where imF (x, y) | x < w1 && y < h1 = f2 (x, y)
        imF (x, y)                    = f1 (x, y)


readPng :: String -> IO Image
readPng path =
  do
    Right im <- readImage path
    let
      img = convertRGB8 im
      dms = (imageWidth img, imageHeight img)
    return $ Image dms $ uncurry (pixelAt img)

writePng :: String -> Image -> IO ()
writePng path (Image (w, h) f) =
  let im = generateImage (curry f) w h
  in P.writePng path im

colorToPixel :: Color -> PixelRGB8
colorToPixel c =
  let V3 r g b = mapv ((* 255.9) . sqrt) c
  in  PixelRGB8 (fromInteger $ floor r)
                (fromInteger $ floor g)
                (fromInteger $ floor b)

-- partialOverwritePng :: Image -> String -> IO ()
-- partialOverwritePng newIm fname =
--   do
--     mim <- readImage fname
--     let im = case mim of
--               Left err -> error err
--               Right im -> im
--     writePng ("./image.png") $ imF
