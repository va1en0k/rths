{-# LANGUAGE ExistentialQuantification, FlexibleInstances, Rank2Types #-}

module Types where

import Data.Vec3
import Control.Monad.Random


data Hitable_ = forall a . Hitable a => MkHitable a

type World = [Hitable_]

data Ray = Ray {origin :: CVec3, direction :: CVec3}
  deriving (Show)



data Hit = Hit {
  hitT :: Double,
  hitP :: CVec3,
  hitNormal :: CVec3,
  scatter :: forall g . RandomGen g => Rand g Scatter
}

class Hitable a where
  hit :: a -> Ray -> Double -> Double -> Maybe Hit

type Color = CVec3




-- Materials
type Scatter = Maybe (CVec3, Ray)

type MaterialScatterF g = RandomGen g => Ray -> Hit -> Rand g Scatter

data Material = Material {scatterF :: forall g. RandomGen g => MaterialScatterF g}
