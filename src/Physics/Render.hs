module Physics.Render where

import Linear.V3

import Types
import Geometry.World

yStep = 0.02

applyGravity :: Sphere -> Sphere
applyGravity s@(Sphere m c r) | r > 3 = s
applyGravity s@(Sphere m (V3 x y z) r) | y > r = Sphere m (V3 x (max r (y - yStep)) z) r
applyGravity s = s

onlyGravity :: [Sphere] -> [Sphere]
onlyGravity = map applyGravity

onlyGravity' :: World -> World
onlyGravity' w = map MkHitable $ onlyGravity (map asSphere w)

makeScenario :: World -> [World]
makeScenario = iterate onlyGravity'
