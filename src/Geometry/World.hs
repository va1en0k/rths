module Geometry.World where

import           Control.Monad.Random
import           Data.Foldable
import           Linear.V3
import           Linear.Metric

import           Geometry.Vectors
import           Geometry.Camera
import           Hitable
import           Material
import           Types

import           RTMonad


sphere m c r = MkHitable $ Sphere m c r
plane m p no = MkHitable $ Plane m p no
triangle m a b c = MkHitable $ Triangle m a b c
polygon m ps = MkHitable $ Polygon m ps

objects :: [Hitable_]
objects =
  [ sphere (mkLambertian $ V3 0.1 0.2 0.5) (V3 0 0 (-1))        0.5
  , sphere (mkLambertian $ V3 0.8 0.8 0.0) (V3 0 (-100.5) (-1)) 100
  , sphere (mkMetal 0.3 $ V3 0.8 0.6 0.2)  (V3 1 0 (-1))        0.5
  ,
  -- sphere (mkMetal 1.0 $ V3 0.8 0.8 0.8) (V3 (-1) 0 (-1)) 0.5
    sphere (mkDielectric 1.5)                 (V3 (-1) 0 (-1))     (-0.45)
  ]

setWorld :: World -> RT ()
setWorld w = updateSettings (\s -> s { world = w })

randomWorld :: RT ()
-- randomWorld = setWorld
--   [ plane (mkLambertian $ CVec3 0.5 0.5 0.5) (V3 0 (-1) (-1)) (V3 0.02 1 (-0.2))
--   , sphere (mkLambertian $ CVec3 0.4 0.2 0.1) (CVec3 (-4) 1 0) 1
--   , sphere (mkDielectric 1.5)                 (CVec3 0 1 0)       1
--   , sphere (mkMetal 0 $ CVec3 0.7 0.6 0.5)    (CVec3 4 1 0)       1
--   ]

red = (mkLambertian $ V3 0.9 0.1 0.1)
blue = (mkLambertian $ V3 0.1 0.1 0.9)
green = (mkLambertian $ V3 0.1 0.9 0.1)

-- cameraF = (getCameraReflectedThroughUV camera)

randomWorld = setWorld $ typical ++ hints -- ((typical ++) <$> concat <$> sequence randList) >>= setWorld
  where
    hints =
      map (\p -> sphere (mkLambertian p) p 0.1)
      [ --(CVec3 0.4 (-0.4) 6)
      --, (CVec3 (-1.3) (-0.4) 6)
      -- , (CVec3 0 (0.3) (-1))
      ] ++
      [ --sphere red cameraF 0.1
      --, triangle red (lv cameraF) (lv $ (cLowerLeftCorner camera + cVertical camera)) (lv cameraF + V3 1 0 0)
      ]

    -- X> Yv Z^

    typical = table ++
      [ sphere (mkMetal 0.1 $ V3 0.7 0.6 0.4) (V3 (0) (-1.6) 8) 1.8
      , sphere (mkLambertian $ V3 0.4 0.2 0.8) (V3 (-2.0) (0.4) 3.5) 0.8
      , sphere (mkDielectric 1.5) (V3 1.1 (0.3) 3.7) 0.85
      ]
    typical' =
      [ --plane (mkLambertian $ V3 0.5 0.5 0.5) (V3 0 (-1) (-1)) (V3 0.02 1 (-0.3))
        -- triangle (mkLambertian $ V3 0.5 0.5 0.5) (V3 0 0 0) (V3 0 (-0.9) 4) (V3 3 (-1.1) 2)
        polygon (mkLambertian $ V3 0.5 0.5 0.5)
        [
        -- , (V3 0.4 (0.3) 4)
        -- , (V3 (-1.6) (0.3) 4)
        -- , (V3 (-1.6) (0.3) 2)
        -- , (V3 0.4 (0.3) 2)
        ]
      --sphere (mkLambertian $ V3 0.5 0.5 0.5) (V3 0 (-1000) 0) 1000
      ,
      -- sphere (mkDielectric 1.5) (V3 0 0 0) 1,
        sphere (mkDielectric 1.5)                 (V3 0 1 0)       1
      , sphere (mkLambertian $ V3 0.4 0.2 0.1) (V3 (-4) 1 0)    1
      , sphere (mkMetal 0 $ V3 0.7 0.6 0.5)    (V3 4 1 0)       1
      ]

    randList :: [RT [Hitable_]]
    randList = (flip map) [ (a, b) | a <- [-10 .. 7], b <- [-8 .. 8] ]
                          (uncurry genSphere)

    genSphere :: Double -> Double -> RT [Hitable_]
    genSphere a b = do
      (mt           : x  : y  : z  : _) <- getRands
      (r1 : r2 : r3 : r4 : r5 : r6 : _) <- getRands
      let center = V3 (a + x * 0.9) (0.2 + y * 5) (b + 0.9 * z)
      if (norm (center - V3 4 0.2 0) > 0.9)
        then return $ return $ case () of
          () | mt < 0.6 ->
            sphere (mkLambertian $ V3 (r1 * r2) (r3 * r4) (r5 * r6)) center 0.2
          () | mt < 0.75 -> sphere
            ( mkMetal (0.5 * r4)
            $ V3 (0.5 * (1 + r1)) (0.5 * (1 + r2)) (0.5 * (1 + r3))
            )
            center
            0.2
          () | otherwise -> sphere (mkDielectric 1.5) center 0.2
        else return []

lv :: CVec3 -> V3 Double
lv = id

bv :: V3 Double -> CVec3
bv = id

-- point, normal
data Plane = Plane Material (V3 Double) (V3 Double)
  deriving (Show)

instance Hitable Plane where
  asSphere line = undefined
  hit p@(Plane m po no) r@(Ray org dir) mn mx =
    let denom = no `dot` lv dir
        t = ((po - lv org) `dot` no) / denom
    in if abs denom > 0.001 && t >= mn && t <= mx
        then Just $ Hit t (rayPointAt r t) (bv no) m
        else Nothing


table =
  let
    mat = mkLambertian $ V3 (209/256) (192/256) (87/256)
    cover =
      [ (V3 0.2 (0.3) 4)
      , (V3 (-1.1) (0.3) 4)
      , (V3 (-1.1) (0.3) 2)
      , (V3 0.2 (0.3) 2)
      ]
    cover_down = (map (+ V3 0 (- 0.4) 0) cover)
    sideIdxs = zip [0..3] (tail $ cycle [0..3])
    sides = map (\(i, j) -> [cover !! i, cover !! j, cover_down !! j, cover_down !! i]) sideIdxs
  in
    [ polygon mat cover
    , polygon mat cover_down
    ] ++ map (polygon mat) sides

data Triangle = Triangle Material (V3 Double) (V3 Double) (V3 Double)
  deriving (Show)

sameSide :: Ray -> (V3 Double) -> (V3 Double) -> Bool
sameSide (Ray o d) a b =
  let cp1 = lv d `cross` (a - lv o)
      cp2 = lv d `cross` (b - lv o)
  in cp1 `dot` cp2 >= 0

instance Hitable Triangle where
  asSphere line = undefined
  hit p@(Triangle m a b c) r@(Ray org dir) mn mx =
    let plane = Plane m a ((b - a) `cross` (c - a))
        sides = [(c, Ray (bv a) $ bv (b - a)), (a, Ray (bv b) $ bv (c - b)), (b, Ray (bv c) $ bv (a - c))]
    in case hit plane r mn mx of
      Nothing -> Nothing
      Just h@(Hit t p n m) ->
        if (and $ map (\(v, s) -> sameSide s v $ lv p) sides)
          then Just h
          else Nothing

data Polygon = Polygon Material [V3 Double]
  deriving (Show)

instance Hitable Polygon where
  asSphere line = undefined
  hit (Polygon m (p:ps)) r mn mx =
    let
      seqPoints = zip ps (tail ps)
      triangles = map (uncurry $ Triangle m p) seqPoints
    in msum (map (\t -> hit t r mn mx) triangles)

instance Hitable Sphere where
  asSphere a = a
  hit (Sphere m sc sr) r@(Ray org dir) mn mx =
    let oc = org - sc
        a = dir `dot` dir
        b = 2 * oc `dot` dir
        c = (oc `dot` oc) - (sr * sr)
        dsc = b * b - 4 * a * c
        hf x = Just h
          where
            p = rayPointAt r x
            -- n = normalize $ p - CVec3 0 0 (-1)
            n = mapv (/ sr) (p - sc)
            h = Hit x p n m
    in if dsc < 0 then Nothing
       else let x0 = ( - b - sqrt dsc ) / (2 * a)
                x1 = ( - b + sqrt dsc ) / (2 * a)
            in if x0 >= mn && x0 <= mx then hf x0
               else if x1 >= mn && x1 <= mx then hf x1
                 else Nothing
