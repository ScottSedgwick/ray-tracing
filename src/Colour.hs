{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Colour (Colour, addColour, colourDepth, packColour, colour, sumColour) where

import           Data.ByteString.Builder (Builder, word8, word16BE)
import           Vec3 (Vec3)

data Colour = Colour
  { rgb :: Vec3
  , samples :: Int
  }

class ColourAddable a where
  addColour :: Colour -> a -> Colour

emptyColour :: Colour
emptyColour = Colour
  { rgb = (0,0,0)
  , samples = 0
  }

colourDepth :: Int
-- colourDepth = 255
colourDepth = 65535

packColour :: Colour -> Builder
packColour Colour {rgb = (r',g',b'), samples = s} = 
    if colourDepth < 256
    then word8 (round r) <> word8 (round g) <> word8 (round b)
    else word16BE (round r) <> word16BE (round g) <> word16BE (round b)
  where
    r = r' // s
    g = g' // s
    b = b' // s

(//) :: Double -> Int -> Double
(//) x 0 = x
(//) x y = x / fromIntegral y

colour :: Vec3 -> Colour
colour (r,g,b) = Colour { rgb = (f r, f g, f b), samples = 1 }
  where
    f x | x < 0 = 0.0
        | x > 1 = fromIntegral colourDepth 
        | otherwise = x * fromIntegral colourDepth

instance ColourAddable Vec3 where
  addColour Colour {rgb = (r1,g1,b1), samples = s} (r2,g2,b2) = Colour
    { rgb = (r1 + r2, g1 + g2, b1 + b2)
    , samples = s + 1
    }

instance ColourAddable Colour where
  addColour Colour {rgb = (r1,g1,b1), samples = s1} Colour {rgb = (r2,g2,b2), samples = s2} = Colour
    { rgb = (r1 + r2, g1 + g2, b1 + b2)
    , samples = s1 + s2
    }

sumColour :: Traversable t => t Colour -> Colour
sumColour = foldr addColour emptyColour