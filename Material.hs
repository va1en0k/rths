module Material where

import Random
import Vectors

albedo :: undefined

lambertian :: Ray -> Hit -> Rand g (Maybe (CVec3, Ray))
lambertian rayIn hit =
  target >>= \t -> Just (albedo, Ray (hitP hit) t)
  -- p + normal + random - p ?
  where target = (hitNormal hit + ) <$> randomInUnitBall

metal :: Ray -> Hit -> Rand g (Maybe (CVec3, Ray))
metal rayIn hit = return
  if didScatter
    then Just (scattered, attenuation)
    else Nothing
  where reflected = reflect (normalize rIn) (hitNormal hit)
        scattered = Ray (hitP hit) reflected
        attenuation = albedo
        didScatter = reflected .* (hitNormal hit) > 0
