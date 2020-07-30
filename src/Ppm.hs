module Ppm (Colour, Ppm, colour, colourDepth, emptyPpm, packPpm, ppm, ppmf) where

import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Builder (Builder, intDec, string7, toLazyByteString, word8, word16BE)
import qualified Data.Vector as V
import           Vec3 (Colour, Point3)

data Ppm = Ppm
  { height :: !Int
  , width :: !Int
  , pixels :: V.Vector Colour
}

colourDepth :: Int
-- colourDepth = 255
colourDepth = 65535

packColour :: Colour -> Builder
packColour (r,g,b) = 
  if colourDepth < 256
  then word8 (round r) <> word8 (round g) <> word8 (round b)
  else word16BE (round r) <> word16BE (round g) <> word16BE (round b)

colour :: Point3 -> Colour
colour (r,g,b) = (f r, f g, f b)
  where
    f x | x < 0 = 0.0
        | x > 1 = fromIntegral colourDepth 
        | otherwise = x * fromIntegral colourDepth

ppm :: Ppm -> [Colour] -> Ppm
ppm p cs = Ppm
  { height = height p
  , width = width p
  , pixels = V.fromList cs
  }

ppmf :: Ppm -> (Point3 -> Colour) -> Ppm
ppmf p f = Ppm
  { height = height p
  , width = width p
  , pixels = V.map f (zplane p)
  }

emptyPpm :: Int -> Int -> Ppm
emptyPpm w h = Ppm
  { height = h
  , width = w
  , pixels = V.empty
  }

packPpm :: Ppm -> B.ByteString
packPpm p = toLazyByteString $ 
  string7 "P6 "
  <> intDec (width p)
  <> string7 " "
  <> intDec (height p)
  <> string7 " "
  <> intDec colourDepth
  <> string7 "\n"
  <> V.foldr (\c acc -> packColour c <> acc) mempty (pixels p)

zplane :: Ppm -> V.Vector Point3
zplane p = V.fromList [ (fromIntegral x, fromIntegral (height p - 1 - y), 0) | y <- [0..height p - 1], x <- [0..width p - 1]]
