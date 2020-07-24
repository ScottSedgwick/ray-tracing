module Main (main) where

import qualified Data.ByteString.Lazy as B
-- import qualified Data.Vector as V

import Vec3 (Point3, Vec3, (<<+), (<<-), (<</), (<<**), dot, unitVec)
import qualified Ppm
import Ray (Ray(..))

main :: IO ()
main = do
  chapter02_1
  chapter02_2
  chapter_04
  chapter_05

chapter02_1 :: IO()
chapter02_1 = do
  let p = Ppm.ppm (Ppm.empty 3 2) $ map Ppm.colour [(65535, 0, 0), (0, 65535, 0), (0, 0, 65535), (65535, 65535, 0), (65535, 65535, 65535), (0, 0, 0)]
  B.writeFile "ppm/chapter02_1.ppm" (Ppm.pack p)

chapter02_2 :: IO()
chapter02_2 = do
  let p = Ppm.ppmf (Ppm.empty 256 256) (\(x,y,_) -> Ppm.colour $ (x, y, 64) <<** 256)
  B.writeFile "ppm/chapter02_2.ppm" (Ppm.pack p)

chapter_04 :: IO()
chapter_04 = generateBackground rayColour_4 "ppm/chapter04.ppm"

chapter_05 :: IO()
chapter_05 = generateBackground rayColour_5 "ppm/chapter05.ppm"

generateBackground :: (Ray -> Ppm.Colour) -> String -> IO()
generateBackground rayColour filename = B.writeFile filename (Ppm.pack p)
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

    p = Ppm.ppmf (Ppm.empty imageWidth imageHeight) renderBg
    
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

rayColour_4 :: Ray -> Ppm.Colour
rayColour_4 r = Ppm.colour ((1,1,1) <<** (1 - t) <<+ (0.5, 0.7, 1) <<** t) <<** fromIntegral Ppm.colourDepth
  where
    (_, uy, _) = unitVec (dirn r)
    t = (uy + 1) * 0.5

rayColour_5 :: Ray -> Ppm.Colour
rayColour_5 r = if hitSphere (0, 0, -1) 0.5 r
                then Ppm.colour (1,0,0) <<** fromIntegral Ppm.colourDepth
                else rayColour_4 r
     
hitSphere :: Point3 -> Double -> Ray -> Bool
hitSphere centre radius ray = discriminant > 0
  where
    discriminant = b * b - 4 * a * c
    a = dot (dirn ray) (dirn ray)
    b = 2 * dot oc (dirn ray)
    c = dot oc oc - radius * radius
    oc = origin ray <<- centre