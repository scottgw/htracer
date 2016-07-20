module Raytrace where

import Image
import Intersect
import Point
import Vector

type Light = (Point, RGB)
type Scene = ([Surface], [Light])

black = RGB (0,0,0)
bgcolour = RGB (255, 0, 255)

-- get a ray from a camera location to a pixel
ray_for_pixel :: Scalar -> Scalar -> Scalar -> Scalar -> Ray
ray_for_pixel w h x y = r
  where r = Ray origin direction
        direction = normalize (delta p origin)
        x' = (x / w) - 0.5
        y' = (y / h) - 0.5
        p = Point (x', y', 1.0)

-- send rays over the scene and return an image.
raytrace :: Scene -> Integer -> Integer -> Image
raytrace scene width height = Image (fromInteger width, fromInteger height, ps)
  where ps = [ get_colour_for_ray scene (ray_for_pixel w h x y) |
               y <- map fromInteger [0..height - 1], x <- map fromInteger [0..width - 1] ]
        w :: Scalar
        w = fromInteger (width - 1)
        h :: Scalar
        h = fromInteger (height - 1)

-- get list of surfaces and Maybe distances to those surfaces which the ray intersects.
get_ray_intersections :: Scene -> Ray -> [(Surface, [Scalar])]
get_ray_intersections scene ray = [ (obj, ray_surface_intersect ray obj) | obj <- fst scene ]

get_colour_for_ray :: Scene -> Ray -> RGB
get_colour_for_ray scene ray = srdetectcolour' scene ray (srintersect scene ray)

srdetectcolour' :: Scene -> Ray -> Maybe (Surface, Scalar) -> RGB
--srdetectcolour' scene (Ray ro rd) (Just (s,d)) = surface_colour s
--srdetectcolour' scene r Nothing = bgcolour
srdetectcolour' scene (Ray ro rd) (Just (s, d)) = lightadded + (surface_colour s)
  where lightsvisible :: [Light]
        lightsvisible = lightsvisiblefrom intersectpoint scene
        lightadded :: RGB
        lightadded = foldr (+) black (map effectivelight lightsvisible)
        effectivelight :: Light -> RGB
        effectivelight (p, c) = rgb_from_list $ map (round . (*10000) . (/ (vectorsum ((delta intersectpoint p) * (delta intersectpoint p)))) . fromInteger) (rgb_to_integer_list c)
        intersectpoint = translate ro (mult d rd)
srdetectcolour' scene r Nothing = bgcolour

lightsvisiblefrom :: Point -> Scene -> [Light]
lightsvisiblefrom x scene@(surfs, lights) = filter isvis lights
  where isvis :: Light -> Bool
        isvis light = pastthelight (srintersect scene (Ray x (delta (fst light) x)))
        pastthelight :: Maybe (Surface, Scalar) -> Bool
        pastthelight Nothing = True
        pastthelight (Just (s, d)) | d > 1 = True
                                   | otherwise = False

-- first surface intersection for ray.
srintersect :: Scene -> Ray -> Maybe (Surface, Scalar)
srintersect scene ray = min_intersection $ flatten_intersections $ get_ray_intersections scene ray

flatten_intersections :: [(Surface, [Scalar])] -> [(Surface, Scalar)]
flatten_intersections ((surf, []):extra) = flatten_intersections extra
flatten_intersections ((surf, xs):extra) = ((flatten_intersections' surf xs) ++ (flatten_intersections extra))
flatten_intersections [] = []

flatten_intersections' :: Surface -> [Scalar] -> [(Surface, Scalar)]
flatten_intersections' surf (x:xs) = (surf, x):(flatten_intersections' surf xs)
flatten_intersections' surf[] = []

min_intersection :: [(Surface, Scalar)] -> Maybe (Surface, Scalar)
min_intersection ((sa, a):(sb, b):extra) | a < b = min_intersection ((sa, a):extra)
                                         | otherwise = min_intersection ((sb, b):extra)
min_intersection [] = Nothing
min_intersection [(sa, a)] = Just (sa, a)
