module Physics.Render where

import Data.Vec3

import Types
import World

yStep = 0.01

applyGravity :: Sphere -> Sphere
applyGravity s@(Sphere m c r) | r > 3 = s
applyGravity s@(Sphere m (CVec3 x y z) r) | y > r = Sphere m (CVec3 x (max r (y - yStep)) z) r
applyGravity s = s

onlyGravity :: [Sphere] -> [Sphere]
onlyGravity = map applyGravity

onlyGravity' :: World -> World
onlyGravity' w = map MkHitable $ onlyGravity (map asSphere w)

makeScenario :: World -> [World]
makeScenario = iterate onlyGravity'
