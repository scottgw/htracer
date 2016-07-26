{-# LANGUAGE BangPatterns #-}

module Intersect (Surface(..), Ray(..), Intersect(..),
                  ray_surface_intersect, surface_colour) where

import Image
import Point
import Vector

data Surface = Sphere !Point !Scalar !RGB deriving Show

data Ray = Ray !Point !Vector deriving Show

-- Represents different kind of intersections (no points, 1 point, 2 points).
data Intersect =
  Intr2 !Scalar !Scalar
  | Intr1 !Scalar
  | Intr0

dscr :: Scalar -> Scalar -> Scalar -> Scalar
dscr a b c = (b * b) - (4.0 * a * c)

solve_quad :: Scalar -> Scalar -> Scalar -> Intersect
solve_quad a b c =
  case (i1 > 0.00001, i2 > 0.000001) of
    (True, True) -> Intr2 i1 i2
    (True, False) -> Intr1 i1
    (False, True) -> Intr1 i2
    _ -> Intr0
  where
    !i1 = ((-b) + sqrtdscr) / (2.0 * a)
    !i2 = ((-b) - sqrtdscr) / (2.0 * a)
    !sqrtdscr = sqrt $ dscr a b c

ray_surface_intersect :: Ray -> Surface -> Intersect
ray_surface_intersect (Ray ro rd) (Sphere sx r col) = solve_quad a b c
  where !se = delta sx ro
        !a = dot2 rd
        !b = -2.0 * dot rd se
        !c = dot2 se - (r ** 2)

surface_colour :: Surface -> RGB
surface_colour (Sphere p r c) = c

test_intersect = abs(a - 57.7350269189626) < 0.00001
  where Intr2 a _ = ray_surface_intersect
                    (Ray origin (Vector 1 1 1))
                    (Sphere origin 100 (RGB 50 50 50))
