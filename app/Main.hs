module Main(main) where

import Intersect
import Image
import Point
import Raytrace(raytrace, Light(..))
import Vector

test_scene = ([ Sphere (Point (Vector 0 0 500)) 100 (RGB 50 70 90),
                Sphere (Point (Vector 100 80 450)) 50 (RGB 250 30 10),
                Sphere (Point (Vector (-100) (-50) 600)) 120 (RGB 200 220 250)],
              [ Light (Point (Vector (-200) (-200) 500)) (RGB 200 10 10),
                Light (Point (Vector 80 (-100) 300)) (RGB 0 40 200),
                Light (Point (Vector 150 (-20) 400)) (RGB 60 120 60) ])

main :: IO ()
main = write_ppm "test.ppm" (raytrace test_scene 1024 1024)
