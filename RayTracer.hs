{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeFamilies #-}

module RayTracer where

import Data.Vec3
import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.Function

import Vectors

data Ray = Ray {origin :: CVec3, direction :: CVec3}
  deriving (Show)

data Hit = Hit {
  hitT :: Double,
  hitP :: CVec3,
  hitNormal :: CVec3
}

class Hitable a where
  hit :: a -> Ray -> Double -> Double -> Maybe Hit

data Hitable_ = forall a . Hitable a => MkHitable a

instance Hitable Hitable_ where
 hit (MkHitable a) r mn mx = hit a r mn mx

instance Hitable [Hitable_] where
  hit objects r mn mx = listToMaybe $ sortBy (compare `on` hitT) $ catMaybes $ map (\o -> hit o r mn mx) objects


maxFloat :: Double
maxFloat = fromIntegral $ snd $ floatRange (0.5::Double)


(*.) :: Double -> CVec3 -> CVec3
k *. v = fromXYZ (x * k, y * k, z * k)
  where (x, y, z) = toXYZ v

-- y :: Vec3 a -> Double
y v = y'
  where (_, y', _) = toXYZ v

rayPointAt :: Ray -> Double -> CVec3
rayPointAt (Ray o d) t = o <+> (t *. d)
