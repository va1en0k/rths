module Graphics.Images where

import           Codec.Picture
import           Data.Array
import           Linear.V3
import           Linear.Metric

import           Geometry.Vectors
import           Types
import           Util

type ImgBuf = Array (Int, Int) PixelRGB8

colorToPixel :: Color -> PixelRGB8
colorToPixel c =
  let V3 r g b = mapv ((* 255.9) . sqrt) c
  in  PixelRGB8 (fromInteger $ floor r)
                (fromInteger $ floor g)
                (fromInteger $ floor b)

partialOverwritePng :: Image PixelRGB8 -> String -> IO ()
partialOverwritePng imF fname =
  do
    mim <- readImage fname
    let im = case mim of
              Left err -> error err
              Right im -> im
    writePng ("./image.png") $ imF
