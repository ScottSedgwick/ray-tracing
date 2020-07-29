module Sphere (Sphere(..)) where

import Hit (HitRecord(..), Hittable(..), setFaceNormal)
import Ray (Ray(..), rayAt)
import Vec3 (Point3, (<<-), (<</), dot, vlenSquared)

data Sphere = Sphere
  { centre :: Point3
  , radius :: Double
  }

instance Hittable Sphere where
  hit ray tmin tmax sphere
      | discriminant <= 0 = Nothing
      | temp0 < tmax && temp0 > tmin = Just (f temp0)
      | temp1 < tmax && temp1 > tmin = Just (f temp1)
      | otherwise = Nothing
    where
      oc = origin ray <<- centre sphere
      a = vlenSquared (dirn ray)
      halfB = dot oc (dirn ray)
      c = vlenSquared oc - radius sphere * radius sphere
      discriminant = halfB * halfB - a * c
      root = sqrt discriminant
      temp0 = ((- halfB) - root) / a
      temp1 = ((- halfB) + root) / a
      f temp = setFaceNormal ray hr
        where
          hr = HitRecord
                { t = temp
                , p = p'
                , normal = (p' <<- centre sphere) <</ radius sphere
                , frontFace = False
                }
          p' = rayAt ray temp