{-# LANGUAGE FlexibleInstances #-}
module Hit (HitRecord(..), Hittable(..), setFaceNormal) where

import Ray (Ray(..))
import Vec3 (Point3, Vec3, (<<**), dot)

data HitRecord = HitRecord
  { p :: Point3
  , normal :: Vec3
  , t :: Double
  , frontFace :: Bool
  }

setFaceNormal :: Ray -> HitRecord -> HitRecord
setFaceNormal ray hitrec = hitrec 
    { frontFace = ff
    , normal = if ff then normal hitrec else normal hitrec <<** (-1)
      }
  where
    ff = dot (dirn ray) (normal hitrec) < 0

class Hittable a where
  hit :: Ray -> Double -> Double -> a -> Maybe HitRecord

instance (Hittable a, Foldable t) => Hittable (t a) where
  hit ray tmin tmax = findmaybe (hit ray tmin tmax)

findmaybe :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findmaybe f = foldl g Nothing
  where
    g Nothing a = f a
    g justa _   = justa