module Main where

import Intersect
import Image
import Point
import Vector

main :: IO ()
main = print $ intersect r s
  where
    r = Ray origin (normalize(Vector (0, 0, 1)))
    s = Sphere (Point (0, 0, 10)) 5.0 (RGB (255, 255, 0))
