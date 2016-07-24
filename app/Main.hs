module Main where

import Intersect
import Image
import Point
import Raytrace(raytrace)
import Vector

test_scene = ([ Sphere (Point (Vector 0 0 500)) 100 (RGB 50 70 90),
                Sphere (Point (Vector 100 80 450)) 50 (RGB 250 30 10),
                Sphere (Point (Vector (-100) (-50) 600)) 120 (RGB 200 220 250)],
              [ (Point (Vector (-200) (-200) 500), (RGB 200 10 10)),
                (Point (Vector 80 (-100) 300), (RGB 0 40 200)),
                (Point (Vector 150 (-20) 400), (RGB 60 120 60)) ])

main :: IO ()
main = writeFile "test.ppm" test
  where
    test = create_ppm $ raytrace test_scene 1024 1024
