module World where

import           Control.Monad.Random
import           Data.Vec3

import           Geometry.Vectors
import           Hitable
import           Material
import           Types

import           RTMonad

sphere m c r = MkHitable $ Sphere m c r


objects :: [Hitable_]
objects =
  [ sphere (mkLambertian $ CVec3 0.1 0.2 0.5) (CVec3 0 0 (-1))        0.5
  , sphere (mkLambertian $ CVec3 0.8 0.8 0.0) (CVec3 0 (-100.5) (-1)) 100
  , sphere (mkMetal 0.3 $ CVec3 0.8 0.6 0.2)  (CVec3 1 0 (-1))        0.5
  ,
  -- sphere (mkMetal 1.0 $ CVec3 0.8 0.8 0.8) (CVec3 (-1) 0 (-1)) 0.5
    sphere (mkDielectric 1.5)                 (CVec3 (-1) 0 (-1))     (-0.45)
  ]

randomWorld :: RT ()
-- randomWorld :: RandomGen g => Rand g World
randomWorld = ((typical ++) <$> concat <$> sequence randList)
  >>= \w -> updateSettings (\s -> s { world = w })
 where
  typical =
    [ sphere (mkLambertian $ CVec3 0.5 0.5 0.5) (CVec3 0 (-1000) 0) 1000
    ,
    -- sphere (mkDielectric 1.5) (CVec3 0 0 0) 1,
      sphere (mkDielectric 1.5)                 (CVec3 0 1 0)       1
    , sphere (mkLambertian $ CVec3 0.4 0.2 0.1) (CVec3 (-4) 1 0)    1
    , sphere (mkMetal 0 $ CVec3 0.7 0.6 0.5)    (CVec3 4 1 0)       1
    ]

  randList :: [RT [Hitable_]]
  randList = (flip map) [ (a, b) | a <- [-11 .. 11], b <- [-11 .. 11] ]
                        (uncurry genSphere)

  genSphere :: Double -> Double -> RT [Hitable_]
  genSphere a b = do
    (mt                : x  : z  : _) <- getRands
    (r1 : r2 : r3 : r4 : r5 : r6 : _) <- getRands
    let center = CVec3 (a + x * 0.9) 0.2 (b + 0.9 * z)
    if (norm (center <-> CVec3 4 0.2 0) > 0.9)
      then return $ return $ case () of
        () | mt < 0.6 ->
          sphere (mkLambertian $ CVec3 (r1 * r2) (r3 * r4) (r5 * r6)) center 0.2
        () | mt < 0.75 -> sphere
          ( mkMetal (0.5 * r4)
          $ CVec3 (0.5 * (1 + r1)) (0.5 * (1 + r2)) (0.5 * (1 + r3))
          )
          center
          0.2
        () | otherwise -> sphere (mkDielectric 1.5) center 0.2
      else return []


instance Hitable Sphere where
  asSphere a = a
  hit s@(Sphere m sc sr) r@(Ray org dir) mn mx =
    let oc = org <-> sc
        a = dir .* dir
        b = 2 * oc .* dir
        c = (oc .* oc) - (sr * sr)
        dsc = b * b - 4 * a * c
        hf x = Just h
          where
            p = rayPointAt r x
            -- n = normalize $ p <-> CVec3 0 0 (-1)
            n = mapv (/ sr) (p <-> sc)
            h = Hit x p n s
    in if dsc < 0 then Nothing
       else let x0 = ( - b - sqrt dsc ) / (2 * a)
                x1 = ( - b + sqrt dsc ) / (2 * a)
            in if x0 >= mn && x0 <= mx then hf x0
               else if x1 >= mn && x1 <= mx then hf x1
                 else Nothing
