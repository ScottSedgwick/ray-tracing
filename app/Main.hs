module Main (main) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V

import Vec3 ((<<*), toVec)
import RayTracing (Ppm(..), packPpm, zplane)

main :: IO ()
main = do
  let ppm = Ppm { 
      height = 256
    , width = 256
    , pixels = V.map (\(x,y,_) -> (y, x, 64) <<* toVec 256) (zplane 256 256 0)
    }
  B.writeFile "ppm/demo.ppm" (packPpm ppm)
