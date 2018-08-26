{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeFamilies              #-}

module Hitable where

import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Vec3

import           Geometry.Vectors
import           Material
import           Types



rayPointAt :: Ray -> Double -> CVec3
rayPointAt (Ray o d) t = o <+> (mapv (* t) d)


instance Hitable Hitable_ where
  hit (MkHitable a) r mn mx = hit a r mn mx
  asSphere (MkHitable a) = asSphere a

instance Hitable [Hitable_] where
  hit objects r mn mx = listToMaybe $ sortBy (compare `on` hitT) $ catMaybes $ map (\o -> hit o r mn mx) objects
  asSphere = asSphere . head
