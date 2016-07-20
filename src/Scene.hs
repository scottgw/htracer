module Scene where

import Image
import Intersect
import Point
import Vector

type Scene = [Surface]

bgc = RGB (255, 0, 255)

-- get a ray from a camera location to a pixel
create_ray :: Point -> Point -> Ray
create_ray o p = Ray o (normalize (delta p o))

intersections :: Scene -> Ray -> [(Surface, Maybe Scalar)]
intersections scene ray = [ (obj, intersect ray obj) | obj <- scene ]

surface_intersection :: Scene -> Ray -> Maybe (Surface, Scalar)
surface_intersection scene ray = fstintersect Nothing (intersections scene ray)
  where fstintersect Nothing ((s, Nothing): xs) = fstintersect Nothing xs
        fstintersect Nothing ((s, Just x): xs) = fstintersect (Just (s,x)) xs
        fstintersect m ((s, Nothing): xs) = fstintersect m xs
        fstintersect (Just (ms, m)) ((s, Just x):xs)
          | m > x = fstintersect (Just (s, x)) xs
          | otherwise = fstintersect (Just (ms, m)) xs
        fstintersect m [] = m

