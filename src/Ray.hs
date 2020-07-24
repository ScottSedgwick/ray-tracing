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

import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Builder (intDec, string7, toLazyByteString, word16BE)
import qualified Data.Vector as V
import           Vec3 (Dirn3, Point3, Vec3, (<<+), (<<*), toVec)

data Ray = Ray
  { origin :: Point3
  , dirn :: Dirn3
  }

rayAt :: Ray -> Double -> Point3
rayAt r t = origin r <<+ dirn r <<* toVec t