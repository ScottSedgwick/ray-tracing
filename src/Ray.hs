{- |
Copyright: (c) 2020 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ray
       ( Ray(..)
       , Point3
       , Dirn3
       , rayAt
       ) where

import           Vec3 (Dirn3, Point3, (<<+), (<<**))

data Ray = Ray
  { origin :: Point3
  , dirn :: Dirn3
  }

rayAt :: Ray -> Double -> Point3
rayAt r t = origin r <<+ dirn r <<** t