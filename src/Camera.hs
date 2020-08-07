module Camera (render, renderAA) where

import Colour (sumColour)
import Vec3 (Point3, Vec3, (<<+), (<<-), (<</), (<<**))
import Ppm (Colour, Ppm, emptyPpm, ppmf)
import Ray (Ray(..))
import System.Random (StdGen, randomRs)

data Camera = Camera
  { aspectRatio :: Double
  , viewportHeight :: Double
  , viewportWidth :: Double
  , focalLength :: Double
  , origin' :: Point3
  , horizontal :: Vec3
  , vertical :: Vec3
  , lowerLeftCorner :: Point3
  }

camera :: Camera
camera = Camera
    { aspectRatio = ratio
    , viewportHeight = height
    , viewportWidth = width
    , focalLength = flength
    , origin' = orig
    , horizontal = horz
    , vertical = vert
    , lowerLeftCorner = orig <<- horz <</ 2 <<- vert <</ 2 <<- (0, 0, flength)
    }
  where
    ratio = 16 / 9
    height = 2
    flength = 1
    orig = (0, 0, 0)
    width = ratio * height
    horz = (width, 0, 0)
    vert = (0, height, 0)

getRay :: Double -> Double -> Ray
getRay u v = Ray 
  { origin = origin' camera
  , dirn = lowerLeftCorner camera <<+ (horizontal camera <<** u) <<+ (vertical camera <<** v) <<- origin' camera
  }

render :: (Ray -> Colour) -> Ppm
render rayColour = ppmf (emptyPpm imageWidth imageHeight) renderBg
  where
    imageWidth = 400 :: Int
    imageHeight = round (fromIntegral imageWidth / aspectRatio camera) :: Int
    
    renderBg (i,j,_) = rayColour r
      where
        u = i / fromIntegral (imageWidth - 1)
        v = j / fromIntegral (imageHeight - 1)
        r = getRay u v
        

renderAA :: StdGen -> (Ray -> Colour) -> Ppm
renderAA g rayColour = ppmf (emptyPpm imageWidth imageHeight) renderBgAA
  where
    imageWidth = 400 :: Int
    imageHeight = round (fromIntegral imageWidth / aspectRatio camera) :: Int
    samplesPerPixel = 100

    renderBgAA (i,j,_) = sumColour cs
      where
        rnds = zip (take samplesPerPixel (randomRs (0, 1) g) :: [Double]) (take samplesPerPixel (randomRs (0, 1) g) :: [Double])
        uvs = map (\(u,v) -> ((i + u) / fromIntegral (imageWidth - 1), (j + v) / fromIntegral (imageHeight - 1))) rnds
        cs = map (rayColour . uncurry getRay) uvs
        
  