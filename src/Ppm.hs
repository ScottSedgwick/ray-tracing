module Ppm (Colour, Ppm, emptyPpm, packPpm, ppm, ppmf) where

import           Colour (Colour, colourDepth, packColour)
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Builder (intDec, string7, toLazyByteString)
import qualified Data.Vector as V
import           Vec3 (Point3)

data Ppm = Ppm
  { height :: !Int
  , width :: !Int
  , pixels :: V.Vector Colour
}

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
