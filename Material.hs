{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module Material where

import Data.Vec3
import Control.Monad.Random

import Random
import Vectors


type MaterialScatterF g = RandomGen g => Ray -> Hit -> Rand g Scatter

data Material = Material {scatterF :: forall g. RandomGen g => MaterialScatterF g}


-- mkLambertian :: RandomGen g => CVec3 -> MaterialScatterF g
-- mkLambertian albedo rayIn hit =
--   target >>= \t -> return $ Just (albedo, Ray (hitP hit) t)
--   -- p + normal + random - p ?
--   where target = (hitNormal hit <+>) <$> randomInUnitBall

reflect :: CVec3 -> CVec3 -> CVec3
-- might it be that it assumes that |n| = 1?
reflect v n = v <-> (2 *. ((v .* n) *. n))


mkLambertian :: CVec3 -> Material
mkLambertian albedo = Material m
  where m rayIn hit =
          target >>= \t -> return $ Just (albedo, Ray (hitP hit) t)
          where
              -- p + normal + random - p ?
            target = (hitNormal hit <+>) <$> randomInUnitBall

mkMetal :: Double -> CVec3 -> Material
mkMetal fuzz albedo = Material m
  where
    m rayIn hit =
      scattered >>=
        \sc ->
          return (if didScatter
                    then Just (attenuation, sc)
                    else Nothing)
        where reflected = reflect (normalize $ direction rayIn) (hitNormal hit)
              scattered = randomInUnitBall >>= \r -> return $ Ray (hitP hit) (reflected <+> (fuzz *. r))
              attenuation = albedo
              didScatter = reflected .* (hitNormal hit) > 0


refract :: CVec3 -> CVec3 -> Double -> Maybe CVec3
-- might it be that it assumes that |n| = 1?
refract v n niOverNt = if d > 0 then Just refr else Nothing
  where
    dt = (normalize v) .* n
    d = 1.0 - niOverNt * niOverNt * (1 - dt * dt)
    refr = (niOverNt *. (v <-> (dt *. n))) <-> (sqrt d *. n)

mkDielectric :: Double -> Material
mkDielectric refIdx = Material m
  where
    m rayIn hit = return $ case refract (direction rayIn) outNorm niOverNt of
                            Just refr -> Just (att, Ray (hitP hit) refr)
                            Nothing -> Nothing
      where
        att = CVec3 1 1 1
        (outNorm, niOverNt) =
          if (direction rayIn .* hitNormal hit) > 0
            then ((-1) *. hitNormal hit, refIdx)
            else (hitNormal hit, 1 / refIdx)


      -- refr =
