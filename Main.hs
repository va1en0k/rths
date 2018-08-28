{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeFamilies              #-}

import           Codec.Picture
import           Control.Monad.Identity
import           Control.Monad.Primitive
import           Control.Monad.Random
import           Data.Array
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Data.Vec3
import           Debug.Trace
import           System.Environment

import qualified Data.Text.IO                  as T

-- import Normal

import           Geometry.Vectors
import           Geometry.Camera
import           Types
-- import Progress
import           World
-- import RayTracer
import           Config

-- import Render (kRes, camera, getRay, ImgBuf, colorToPixel)



import           RTMonad

-- import WebGL



import           Material


{-
import Render
main :: IO ()
main = do
  world <- runRT (Settings undefined undefined undefined) (randomWorld >> getWorld)
  -- let world = randomWorld
  -- let world = objects
  imF <- genImageF world (fst res) (snd res)
  let im = (generateImage imF (fst res) (snd res))
  -- im <- evalRandIO rim
  -- print (renderUV 0 0)
  -- print (imF 10 10)
  -- render 10 [] 10 10 >>= print
  -- print (colorToPixel $ CVec3 0.8 0.7 1.0)
  now <- getPOSIXTime
  hash <- head <$> getArgs
  writePng ("./out/image__" ++ (show now) ++ "__" ++ hash ++ ".png") im
  writePng ("./image.png") im
-}

import           Parallel.Render

import Physics.Render

writeGif fname images = case writeGifAnimation fname 5 LoopingForever images of
  Left s -> putStrLn s
  Right a -> a

main :: IO ()
main = do
  print camera
  print $ getRayNormPersp camera 0.5 0.5
  print $ getRayRevPersp camera 0.5 0.5
  world <- runRT (Settings undefined undefined undefined) $ randomWorld >> getWorld
  -- let scenario = [(!! 1000) $ makeScenario world]
  -- let world = randomWorld
  -- let world = objects
  images <- (flip mapM) (zip [1..] [world]) $ \(i, w) ->
    do
      print $ "--- " ++ show i ++ " ---"
      imF <-
        runRT (Settings w undefined undefined) $ uncurry genImageF res
      let im = (generateImage imF (fst res) (snd res))
      return im

  now  <- getPOSIXTime
  hash <- head <$> getArgs
  writeGif ("./out/image__" ++ (show now) ++ "__" ++ hash ++ ".gif") images
  writeGif ("./image.gif") images


{-
main'' :: IO ()
main'' = do
  putStrLn $ types
  putStrLn $ cameraToWebgl camera
  putStrLn $ sphereToWebgl 1 (Sphere (mkLambertian $ CVec3 0.1 0.2 0.5) (CVec3 0 0 (-1)) 0.5)
-}
