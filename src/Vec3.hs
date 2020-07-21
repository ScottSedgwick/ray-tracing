module Vec3 
  ( Vec3
  , (<<+)
  , (<<-)
  , (<<*)
  , dot
  , cross
  , toVec
  , (<</)
  , unitVec
  ) where

import Data.Word (Word16)

type Vec3   = (Word16, Word16, Word16)

(<<+) :: Vec3 -> Vec3 -> Vec3
(<<+) (x0,y0,z0) (x1,y1,z1) = (x0 + x1, y0 + y1, z0 + z1)

(<<-) :: Vec3 -> Vec3 -> Vec3
(<<-) (x0,y0,z0) (x1,y1,z1) = (x0 - x1, y0 - y1, z0 - z1)

(<<*) :: Vec3 -> Vec3 -> Vec3
(<<*) (x0,y0,z0) (x1,y1,z1) = (x0 * x1, y0 * y1, z0 * z1)

(<</) :: Vec3 -> Word16 -> Vec3
(<</) (x,y,z) t = (x `div` t, y `div` t, z `div` t)

infixl 7  <<*, <</
infixl 6  <<+, <<-

dot :: Vec3 -> Vec3 -> Vec3
dot = (<<*)

cross :: Vec3 -> Vec3 -> Vec3
cross (x0,y0,z0) (x1,y1,z1) = 
  ( y0 * z1 - z0 * y1
  , z0 * x1 - x0 * z1
  , x0 * y1 - y0 * x1
  );

toVec :: Word16 -> Vec3
toVec n = (n, n, n)

unitVec :: Vec3 -> Vec3
unitVec v = v <</ vlen v

vlen :: Vec3 -> Word16
vlen = round . sqrt . fromIntegral . vlenSquared

vlenSquared :: Vec3 -> Word16
vlenSquared (x,y,z) = x*x + y*y + z*z
