module Vec3 
  ( Colour
  , Dirn3
  , Point3
  , Vec3
  , (<<+)
  , (<<-)
  , (<<*)
  , (<<**)
  , dot
  , cross
  , toVec
  , (<</)
  , unitVec
  ) where

type Vec3   = (Double, Double, Double)
type Colour = Vec3
type Point3 = Vec3
type Dirn3  = Vec3

(<<+) :: Vec3 -> Vec3 -> Vec3
(<<+) (x0,y0,z0) (x1,y1,z1) = (x0 + x1, y0 + y1, z0 + z1)

(<<-) :: Vec3 -> Vec3 -> Vec3
(<<-) (x0,y0,z0) (x1,y1,z1) = (x0 - x1, y0 - y1, z0 - z1)

(<<*) :: Vec3 -> Vec3 -> Vec3
(<<*) (x0,y0,z0) (x1,y1,z1) = (x0 * x1, y0 * y1, z0 * z1)

(<<**) :: Vec3 -> Double -> Vec3
(<<**) (x,y,z) m = (x * m, y * m, z * m)

(<</) :: Vec3 -> Double -> Vec3
(<</) (x,y,z) t = (x / t, y / t, z / t)

infixl 7  <<*, <</, <<**
infixl 6  <<+, <<-

dot :: Vec3 -> Vec3 -> Double
dot (x0, y0, z0) (x1, y1, z1) = x0 * x1 + y0 * y1 + z0 * z1

cross :: Vec3 -> Vec3 -> Vec3
cross (x0,y0,z0) (x1,y1,z1) = 
  ( y0 * z1 - z0 * y1
  , z0 * x1 - x0 * z1
  , x0 * y1 - y0 * x1
  );

toVec :: Double -> Vec3
toVec n = (n, n, n)

unitVec :: Vec3 -> Vec3
unitVec v = v <</ vlen v

vlen :: Vec3 -> Double
vlen = sqrt . vlenSquared

vlenSquared :: Vec3 -> Double
vlenSquared (x,y,z) = x*x + y*y + z*z
