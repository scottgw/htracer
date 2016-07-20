module Intersect where

import Image
import Point
import Vector

data Surface = Sphere Point Scalar RGB deriving Show

data Ray = Ray Point Vector deriving Show

dscr :: Scalar -> Scalar -> Scalar -> Scalar
dscr a b c = (b * b) - (4.0 * a * c)

solve_quad :: Scalar -> Scalar -> Scalar -> [Scalar]
solve_quad a b c = [((-b) + sqrtdscr) / (2.0 * a), ((-b) - sqrtdscr) / (2.0 * a)]
  where
    sqrtdscr = sqrt $ dscr a b c

ray_surface_intersect :: Ray -> Surface -> [Scalar]
ray_surface_intersect (Ray ro rd) (Sphere sx r col) = intersections
  where intersections = filter (> 0.00001) $ solve_quad a b c
        se = delta sx ro
        a = vectorsum (rd * rd)
        b = -2.0 * vectorsum (rd * se)
        c = vectorsum (se * se) - (r * r)

surface_colour :: Surface -> RGB
surface_colour (Sphere p r c) = c

test_intersect = abs(a - 57.7350269189626) < 0.00001
  where (a:as) = (ray_surface_intersect (Ray origin (Vector (1,1,1)))
                                        (Sphere origin 100 (RGB (50,50,50))))
