module World where

import Data.Vec3
import Control.Monad.Random

import Hitable
import Vectors
import Material

import RTMonad

type World = [Hitable_]

objects :: [Hitable_]
objects = [
  MkHitable $ Sphere (mkLambertian $ CVec3 0.1 0.2 0.5) (CVec3 0 0 (-1)) 0.5,
  MkHitable $ Sphere (mkLambertian $ CVec3 0.8 0.8 0.0) (CVec3 0 (-100.5) (-1)) 100,
  MkHitable $ Sphere (mkMetal 0.3 $ CVec3 0.8 0.6 0.2) (CVec3 1 0 (-1)) 0.5,
  -- MkHitable $ Sphere (mkMetal 1.0 $ CVec3 0.8 0.8 0.8) (CVec3 (-1) 0 (-1)) 0.5
  MkHitable $ Sphere (mkDielectric 1.5) (CVec3 (-1) 0 (-1)) (-0.45)
  ]

-- randomWorld :: RT ()
randomWorld :: RandomGen g => Rand g [Hitable_]
randomWorld = return typical --  (typical++) <$> concat <$> sequence randList
  where
    typical = [
      MkHitable $ Sphere (mkLambertian $ CVec3 0.5 0.5 0.5) (CVec3 0 (-1000) 0) 1000,
      MkHitable $ Sphere (mkDielectric 1.5) (CVec3 0 1 0) 1,
      MkHitable $ Sphere (mkLambertian $ CVec3 0.4 0.2 0.1) (CVec3 (-4) 1 0) 1,
      MkHitable $ Sphere (mkMetal 0 $ CVec3 0.7 0.6 0.5) (CVec3 4 1 0) 1]
    -- randList :: [Rand g [Hitable_]]
    randList :: RandomGen g => [Rand g [Hitable_]]
    randList = (flip map) [(a, b) | a <- [-11..11], b <- [-11..11]] (uncurry genSphere)

    genSphere :: RandomGen g => Double -> Double -> Rand g [Hitable_]
    genSphere a b =
      do
        (mt:x:z:_) <- getRandomRs (0, 1)
        (r1:r2:r3:r4:r5:r6:_) <- getRandomRs (0, 1)
        let center = CVec3 (a + x * 0.9) 0.2 (b + 0.9 * z)
        if (norm (center <-> CVec3 4 0.2 0) > 0.9)
          then return $ return $ case () of
                                  () | mt < 0.6 ->
                                      MkHitable $ Sphere (mkLambertian $ CVec3 (r1 * r2) (r3 * r4) (r5 * r6)) center 0.2
                                  () | mt < 0.75 ->
                                      MkHitable $ Sphere (mkMetal (0.5 * r4) $ CVec3 (0.5 * (1 + r1)) (0.5 * (1 + r2)) (0.5 * (1 + r3))) center 0.2
                                  () | otherwise -> MkHitable $ Sphere (mkDielectric 1.5) center 0.2
          else return []

data Sphere = Sphere Material CVec3 Double

instance Hitable Sphere where
  hit (Sphere m sc sr) r@(Ray org dir) mn mx =
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
            h = Hit x p n (scatterF m r h)
    in if dsc < 0 then Nothing
       else let x0 = ( - b - sqrt dsc ) / (2 * a)
                x1 = ( - b + sqrt dsc ) / (2 * a)
            in if x0 >= mn && x0 <= mx then hf x0
               else if x1 >= mn && x1 <= mx then hf x1
                 else Nothing
