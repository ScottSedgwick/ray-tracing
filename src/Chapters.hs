module Chapters 
  ( chapter02_1
  , chapter02_2
  , chapter04
  , chapter05
  , chapter06_1
  , chapter06_2
  ) where

import qualified Data.ByteString.Lazy as B
-- import qualified Data.Vector as V

import Camera (camera)
import Hit (HitRecord(..), Hittable(..))
import Vec3 (Point3, (<<+), (<<-), (<<**), dot, unitVec, vlenSquared)
import qualified Ppm
import Ray (Ray(..), rayAt)
import Sphere (Sphere(..))
import Utils (infinity)

chapter02_1 :: IO()
chapter02_1 = do
  let ppm = Ppm.ppm (Ppm.empty 3 2) [(65535, 0, 0), (0, 65535, 0), (0, 0, 65535), (65535, 65535, 0), (65535, 65535, 65535), (0, 0, 0)]
  B.writeFile "ppm/chapter02_1.ppm" (Ppm.pack ppm)

chapter02_2 :: IO()
chapter02_2 = do
  let ppm = Ppm.ppmf (Ppm.empty 256 256) (\(x,y,_) -> (x, y, 64) <<** 256)
  B.writeFile "ppm/chapter02_2.ppm" (Ppm.pack ppm)

chapter04 :: IO()
chapter04 = generateFile "ppm/chapter04.ppm" $ camera rayColour_4

chapter05 :: IO()
chapter05 = generateFile "ppm/chapter05.ppm" $ camera rayColour_5 

chapter06_1 :: IO()
chapter06_1 = generateFile "ppm/chapter06.ppm" $ camera rayColour6_1 

generateFile :: String -> Ppm.Ppm -> IO()
generateFile filename ppm = B.writeFile filename (Ppm.pack ppm)

rayColour_4 :: Ray -> Ppm.Colour
rayColour_4 r = Ppm.colour ((1,1,1) <<** (1 - t') <<+ (0.5, 0.7, 1) <<** t')
  where
    (_, uy, _) = unitVec (dirn r)
    t' = (uy + 1) * 0.5

rayColour_5 :: Ray -> Ppm.Colour
rayColour_5 r = if hitSphere_5 (0, 0, -1) 0.5 r
                then Ppm.colour (1,0,0)
                else rayColour_4 r
     
hitSphere_5 :: Point3 -> Double -> Ray -> Bool
hitSphere_5 cntr rds ray = discriminant > 0
  where
    discriminant = b * b - 4 * a * c
    a = dot (dirn ray) (dirn ray)
    b = 2 * dot oc (dirn ray)
    c = dot oc oc - rds * rds
    oc = origin ray <<- cntr

hitSphere6_1 :: Point3 -> Double -> Ray -> Double
hitSphere6_1 cntr rds ray =
    if discriminant < 0
    then -1
    else ((-halfB) - sqrt discriminant) / a
  where
    oc = origin ray <<- cntr
    a = vlenSquared (dirn ray)
    halfB = dot oc (dirn ray)
    c = vlenSquared oc - rds * rds
    discriminant = halfB * halfB - a * c

rayColour6_1 :: Ray -> Ppm.Colour
rayColour6_1 ray = 
  if t0 > 0
  then Ppm.colour (n <<+ (1,1,1) <<** 0.5)
  else Ppm.colour ((1,1,1) <<** (1 - t1) <<+ (0.5, 0.7, 1.0) <<** t1)
  where
    t0 = hitSphere6_1 (0, 0, -1) 0.5 ray
    n = unitVec (rayAt ray t0 <<- (0, 0, -1))
    (_,y,_) = unitVec (dirn ray)
    t1 = (y + 1) * 0.5

chapter06_2 :: IO()
chapter06_2 = generateFile "ppm/chapter06_2.ppm" $ camera (rayColour6_2 world)
  where
    world = 
      [ Sphere { centre = (0, 0, -1), radius = 0.5 }
      , Sphere { centre = (0, -100.5, -1), radius = 100 }
      ]
    

rayColour6_2 :: (Hittable a, Foldable t) => t a -> Ray -> Ppm.Colour
rayColour6_2 world ray = f $ hit ray 0 infinity world
  where
    f Nothing = bgcolor ray
    f (Just hitrec) = Ppm.colour $ normal hitrec <<+ (1,1,1) <<** 0.5

bgcolor :: Ray -> Ppm.Colour
bgcolor ray = Ppm.colour ((1,1,1) <<** (1 - t1) <<+ (0.5, 0.7, 1.0) <<** t1)
  where
    (_,y,_) = unitVec (dirn ray)
    t1 = (y + 1) * 0.5