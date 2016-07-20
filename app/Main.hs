module Main where

import Intersect
import Image
import Point
import Scene(raytrace)
import Vector

test_scene = [ Sphere (Point (0, 0, 500)) 100 (RGB (50, 70, 90)),
               Sphere (Point (80, 100, 450)) 50 (RGB (250, 30, 10)),
               Sphere (Point (-50, -100, 600)) 120 (RGB (200, 220, 250)) ]

main :: IO ()
main = writeFile "test.ppm" test
  where
    test = create_ppm $ raytrace test_scene 1024 1024
