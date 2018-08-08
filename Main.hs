{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeFamilies #-}

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
import Render



-- vsum xs = foldr1 (<+>) xs
--
-- renderAntialiased :: Int -> Int -> PixelRGB8
-- renderAntialiased x y = colorToPixel $ (1 / (fromIntegral $ length points)) *. vsum points
--   where
--     points =
--       do i <- [0.1..0.9]
--          j <- [0.1..0.9]
--          return $ renderUV ((i + fromIntegral x) / kRes) ((j + fromIntegral y) / kRes)




main :: IO ()
main = do
  imF <- genImageF (fst res) (snd res)
  let im = (generateImage imF (fst res) (snd res))
  -- im <- evalRandIO rim
  -- print (renderUV 0 0)
  -- print (imF 10 10)
  -- render 10 [] 10 10 >>= print
  -- print (colorToPixel $ CVec3 0.8 0.7 1.0)
  now <- getPOSIXTime
  hash <- head <$> getArgs
  writePng ("./out/image__" ++ hash ++ "__" ++ (show now) ++ ".png") im
  writePng ("./image.png") im
