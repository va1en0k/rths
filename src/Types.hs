{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}

module Types where

import           Control.Monad.Random
import           Linear.V3
import           Parallel.Shaders

type CVec3 = V3 Double

data Hitable_ = forall a . (Show a, Hitable a) => MkHitable a

instance Show Hitable_ where
  show (MkHitable a) = show a

type World = [Hitable_]

data Ray = Ray {origin :: CVec3, direction :: CVec3}
  deriving (Show)



data Hit = Hit {
  hitT      :: Double,
  hitP      :: CVec3,
  hitNormal :: CVec3,
  -- scatter :: forall g . RandomGen g => Rand g Scatter
  hitMaterial :: Material
} deriving (Show)
--
-- instance Show Hit where
--   show (Hit t p n _) = "Hit " ++ show t ++ " (" ++ show p ++ ") (" ++ show n ++ ")"

data Sphere = Sphere Material CVec3 Double
  deriving Show


class Hitable a where
  hit :: a -> Ray -> Double -> Double -> Maybe Hit
  asSphere :: a -> Sphere

type Color = CVec3


-- Materials

data Material = Material {scatterF :: Ray -> Hit -> RT (Maybe (Color, Ray))}

instance Show Material where
  show a = "(Material)"

data RT a = RT (Settings -> IO (a, Settings))

data Settings = Settings {
  world          :: World,
  shaderEngine   :: ShaderEngine,
  rayTraceShader :: Shader
}
