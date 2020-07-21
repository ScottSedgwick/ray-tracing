{- |
Copyright: (c) 2020 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RayTracing
       ( Ppm(..)
       , Ray(..)
       , Point3
       , Dirn3
       , Colour
       , packPpm
       , rayAt
       , zplane
       ) where

import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Builder (string7, toLazyByteString, word16BE)
import qualified Data.Vector as V
import           Data.Word (Word16)
import           Vec3 (Vec3, (<<+), (<<*), toVec)

data Ppm = Ppm
  { height :: !Int
  , width :: !Int
  , pixels :: V.Vector Colour
}

type Point3 = Vec3
type Dirn3  = Vec3
type Colour = Vec3

data Ray = Ray
  { origin :: Point3
  , dirn :: Dirn3
  }

rayAt :: Ray -> Word16 -> Point3
rayAt r t = origin r <<+ dirn r <<* toVec t

packPpm :: Ppm -> B.ByteString
packPpm ppm = toLazyByteString $ 
  string7 "P6 "
  <> string7 (show (width ppm))
  <> string7 " "
  <> string7 (show (height ppm))
  <> string7 " 65535\n"
  <> V.foldr (\(r,g,b) acc -> acc <> word16BE r <> word16BE g <> word16BE b) mempty (pixels ppm)

zplane :: Word16 -> Word16 -> Word16 -> V.Vector Point3
zplane w h z = V.fromList [ (x, h - 1 - y, z) | x <- [0..w - 1], y <- [0..h - 1]]