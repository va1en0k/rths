{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Material where

import           Control.Monad.Random
import           Linear.V3
import           Linear.Metric

import           Geometry.Vectors
import           Random
import           RTMonad
import           Types

-- data Material = Material {scatterF :: Ray -> Hit -> RT (Maybe (Color, Ray))}


-- mkLambertian :: RandomGen g => CVec3 -> MaterialScatterF g
-- mkLambertian albedo rayIn hit =
--   target >>= \t -> return $ Just (albedo, Ray (hitP hit) t)
--   -- p + normal + random - p ?
--   where target = (hitNormal hit +) <$> randomInUnitBall

reflect :: CVec3 -> CVec3 -> CVec3
-- might it be that it assumes that |n| = 1?
reflect v n = v - (2 *. ((v `dot` n) *. n))


mkLambertian :: CVec3 -> Material
mkLambertian albedo = Material m
 where
  m rayIn hit = target >>= \t -> return $ Just (albedo, Ray (hitP hit) t)
    where
        -- p + normal + random - p ?
          target = (hitNormal hit +) <$> randomInUnitBall''

mkMetal :: Double -> CVec3 -> Material
mkMetal fuzz albedo = Material m
 where
  m rayIn hit = scattered
    >>= \sc -> return (if didScatter then Just (attenuation, sc) else Nothing)
   where
    reflected = reflect (normalize $ direction rayIn) (hitNormal hit)
    scattered = randomInUnitBall''
      >>= \r -> return $ Ray (hitP hit) (reflected + (fuzz *. r))
    attenuation = albedo
    didScatter  = reflected `dot` (hitNormal hit) > 0


refract :: CVec3 -> CVec3 -> Double -> Maybe CVec3
-- might it be that it assumes that |n| = 1?
refract v n niOverNt = if d > 0 then Just refr else Nothing
 where
  uv   = normalize v
  dt   = uv `dot` n
  d    = 1.0 - niOverNt * niOverNt * (1 - dt * dt)
  refr = (niOverNt *. (uv - (dt *. n))) - (sqrt d *. n)

schlick :: Double -> Double -> Double
schlick cs refIdx = r02 + (1 - r02) * ((1 - cs) ** 5)
 where
  r0  = (1 - refIdx) / (1 + refIdx)
  r02 = r0 * r0

mkDielectric :: Double -> Material
mkDielectric refIdx = Material m
 where
  m rayIn hit = Just <$> res
   where
    att                         = V3 1 1 1
    drayInDPhit                 = direction rayIn `dot` hitNormal hit
    cosine'                     = drayInDPhit / norm (direction rayIn)
    (outNorm, niOverNt, cosine) = if drayInDPhit > 0
      then
        ( (-1) *. hitNormal hit
        , refIdx
        , sqrt (1 - refIdx * refIdx * (1 - cosine' * cosine'))
        )
      else (hitNormal hit, 1 / refIdx, -cosine')
    (reflProb, refr) = case refract (direction rayIn) outNorm niOverNt of
      Just refr -> (schlick cosine refIdx, refr) --Just (att, Ray (hitP hit) refr)
      Nothing   -> (1.0, undefined)
    res = do
      x <- getRand
      return $ if (x < reflProb)
        then (att, Ray (hitP hit) (reflect (direction rayIn) (hitNormal hit)))
        else (att, Ray (hitP hit) refr)


      -- refr =
