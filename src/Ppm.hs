module Ppm (Colour, Ppm, colour, colourDepth, empty, pack, ppm, ppmf) where

import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Builder (intDec, string7, toLazyByteString, word16BE)
import qualified Data.Vector as V
import           Vec3 (Colour, Point3)

data Ppm = Ppm
  { height :: !Int
  , width :: !Int
  , colours :: !Int
  , pixels :: V.Vector Colour
}

colourDepth :: Int
colourDepth = 65535

colour :: Point3 -> Colour
colour (r,g,b) = (f r, f g, f b)
  where
    f x | x < 0 = 0.0
        | x > fromIntegral colourDepth = fromIntegral colourDepth 
        | otherwise = x

ppm :: Ppm -> [Colour] -> Ppm
ppm p cs = Ppm
  { height = height p
  , width = width p
  , colours = colours p
  , pixels = V.fromList cs
  }

ppmf :: Ppm -> (Point3 -> Colour) -> Ppm
ppmf p f = Ppm
  { height = height p
  , width = width p
  , colours = colours p
  , pixels = V.map f (zplane p)
  }

empty :: Int -> Int -> Ppm
empty w h = Ppm
  { height = h
  , width = w
  , colours = colourDepth
  , pixels = V.empty
  }

pack :: Ppm -> B.ByteString
pack p = toLazyByteString $ 
  string7 "P6 "
  <> intDec (width p)
  <> string7 " "
  <> intDec (height p)
  <> string7 " "
  <> intDec (colours p)
  <> string7 "\n"
  <> V.foldr (\(r,g,b) acc -> word16BE (round r) <> word16BE (round g) <> word16BE (round b) <> acc) mempty (pixels p)

zplane :: Ppm -> V.Vector Point3
zplane p = V.fromList [ (fromIntegral x, fromIntegral (height p - 1 - y), 0) | y <- [0..height p - 1], x <- [0..width p - 1]]
