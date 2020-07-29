module Utils (infinity) where

adouble :: Double
adouble = 1

infinity :: Double
infinity = fromIntegral (snd (floatRange adouble))