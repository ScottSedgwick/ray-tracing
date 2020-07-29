module Camera (camera) where

import Vec3 (Point3, Vec3, (<<+), (<<-), (<</), (<<**))
import qualified Ppm
import Ray (Ray(..))

camera :: (Ray -> Ppm.Colour) -> Ppm.Ppm
camera rayColour = Ppm.ppmf (Ppm.empty imageWidth imageHeight) renderBg
  where
    aspectRatio = 16 / 9 :: Double
    imageWidth = 400 :: Int
    imageHeight = round (fromIntegral imageWidth / aspectRatio) :: Int

    viewportHeight = 2 :: Double
    viewportWidth = aspectRatio * viewportHeight
    focalLength = 1 :: Double

    origin' = (0, 0, 0) :: Point3
    horizontal = (viewportWidth, 0, 0) :: Vec3
    vertical = (0, viewportHeight, 0) :: Vec3
    lowerLeftCorner = origin' <<- (horizontal <</ 2) <<- (vertical <</ 2) <<- (0, 0, focalLength)
    
    renderBg (i,j,_) =
      let
        u = i / fromIntegral (imageWidth - 1)
        v = j / fromIntegral (imageHeight - 1)
        r = Ray 
            { origin = origin'
            , dirn = lowerLeftCorner <<+ (horizontal <<** u) <<+ (vertical <<** v) <<- origin'
            }
      in 
        rayColour r