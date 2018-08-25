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

import qualified Data.Text.IO as T

-- import Normal

import Vectors
import Types
import Progress
import World
import RayTracer

import Render (res, kRes, camera, getRay, ImgBuf, colorToPixel)



import RTMonad

import WebGL



import Material


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

import Parallel.Render
main :: IO ()
main = do
  -- let world = randomWorld >> getWorld)
  -- let world = randomWorld
  -- let world = objects
  imF <- runRT (Settings undefined undefined undefined) $ randomWorld >> genImageF (fst res) (snd res)
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


main'' :: IO ()
main'' = do
  putStrLn $ types
  putStrLn $ cameraToWebgl camera
  putStrLn $ sphereToWebgl 1 (Sphere (mkLambertian $ CVec3 0.1 0.2 0.5) (CVec3 0 0 (-1)) 0.5)
