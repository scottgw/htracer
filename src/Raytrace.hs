{-# LANGUAGE BangPatterns #-}
module Raytrace (Scene, Light(..), raytrace) where

import qualified Data.Array.Repa as Repa
import Data.Array.Repa ((:.)(..))
import Data.List
import Data.Function
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as V

import Image
import Intersect
import Point
import Vector

data Light = Light !Point !RGB
type Scene = ([Surface], [Light])

black = RGB 0 0 0
bgcolour = RGB 255 0 255

-- get a ray from a camera location to a pixel
{-# INLINE ray_for_pixel #-}
ray_for_pixel :: Scalar -> Scalar -> Scalar -> Scalar -> Ray
ray_for_pixel !w !h !x !y = r
  where !r = Ray origin direction
        !direction = normalize (delta p origin)
        x' = (x / w) - 0.5
        y' = (y / h) - 0.5
        p = Point (Vector x'  y' 1.0)

-- send rays over the scene and return an image.
raytrace :: Scene -> Int -> Int -> Image
raytrace scene width height = Image (width, height, Repa.toUnboxed rgbArray)
  where rgbArray :: Repa.Array Repa.U Repa.DIM2 (Int, Int, Int)
        rgbArray = runIdentity (Repa.computeP delayedRgbArray)

        rgbToTuple (RGB r g b) = (r, g, b)

        {-# INLINE go #-}
        go :: Repa.DIM2 -> RGB
        go (Repa.Z :. x :. y) =
          get_colour_for_ray scene (ray_for_pixel sw sh (fromIntegral x) (fromIntegral  y))

        delayedRgbArray :: Repa.Array Repa.D Repa.DIM2 (Int, Int, Int)
        delayedRgbArray =
          Repa.fromFunction (Repa.ix2 width height) (rgbToTuple . go)

        sw :: Scalar
        sw = fromIntegral (width - 1)
        sh :: Scalar
        sh = fromIntegral (height - 1)

data SurfInt = SurfInt Surface !Intersect

-- get list of surfaces and Maybe distances to those surfaces which the ray intersects.
get_ray_intersections :: Scene -> Ray -> [SurfInt]
get_ray_intersections scene !ray =
  map (\obj -> SurfInt obj (ray_surface_intersect ray obj)) (fst scene)

{-# INLINE get_colour_for_ray #-}
get_colour_for_ray :: Scene -> Ray -> RGB
get_colour_for_ray scene ray = srdetectcolour' scene ray (srintersect scene ray)

srdetectcolour' :: Scene -> Ray -> Maybe (Surface, Scalar) -> RGB
--srdetectcolour' scene (Ray ro rd) (Just (s,d)) = surface_colour s
--srdetectcolour' scene r Nothing = bgcolour
srdetectcolour' scene (Ray ro rd) (Just (s, d)) = lightadded + (surface_colour s)
  where lightsvisible :: [Light]
        lightsvisible = lightsvisiblefrom intersectpoint scene

        lightadded :: RGB
        lightadded = foldl' (+) black (map effectivelight lightsvisible)

        colourTransform :: Point -> Int -> Int
        colourTransform !p = round
                            . (* 10000)
                            . (/ (dot2 (delta intersectpoint p)))
                            . fromIntegral

        effectivelight :: Light -> RGB
        effectivelight (Light !p (RGB r g b)) =
          RGB (colourTransform p r) (colourTransform p g) (colourTransform p b)

        intersectpoint = translate ro (mult d rd)
srdetectcolour' scene r Nothing = bgcolour

lightsvisiblefrom :: Point -> Scene -> [Light]
lightsvisiblefrom x scene@(surfs, lights) = filter isvis lights
  where isvis :: Light -> Bool
        isvis (Light p _) = pastthelight (srintersect scene (Ray x (delta p x)))

        pastthelight :: Maybe (Surface, Scalar) -> Bool
        pastthelight (Just (s, d)) | d <= 1 = False
        pastthelight _ = True

-- first surface intersection for ray.
srintersect :: Scene -> Ray -> Maybe (Surface, Scalar)
srintersect scene ray = find_min_intersection $ get_ray_intersections scene ray

find_min_intersection :: [SurfInt] -> Maybe (Surface, Scalar)
find_min_intersection xs = foldl' go Nothing xs
  where
    go :: Maybe (Surface, Scalar) -> SurfInt -> Maybe (Surface, Scalar)
    go acc@(Just (accS, !accI)) (SurfInt s !inter) =
      case inter of
        Intr0 -> acc
        Intr1 a -> if a < accI
                   then Just (s, a)
                   else acc
        Intr2 a b ->
          let c = min a b
          in if c < accI
             then Just (s, c)
             else acc
    go Nothing (SurfInt !s !inter) =
      case inter of
        Intr0 -> Nothing
        Intr1 a ->  Just (s, a)
        Intr2 a b -> Just (s, min a b)


