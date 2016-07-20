module Intersect where

import Image
import Point
import Vector

data Surface = Sphere Point Scalar RGB deriving Show

data Ray = Ray Point Vector deriving Show

dscr :: Scalar -> Scalar -> Scalar -> Scalar
dscr a b c = (b * b) - (4.0 * a * c)

solveQuad :: Scalar -> Scalar -> Scalar -> [Scalar]
solveQuad a b c = [((-b) + sqrtdscr) / (2.0 * a), ((-b) - sqrtdscr) / (2.0 * a)]
  where
    sqrtdscr = sqrt $ dscr a b c

intersect :: Ray -> Surface -> Maybe Scalar
intersect (Ray ro rd) (Sphere sx r col)
  | intersect' == [] = Nothing
  | otherwise = Just (minimum intersect')
  where intersect' | dscr a b c < 0 = []
                   | otherwise = filter (>0.00001) $ solveQuad a b c
                       where
                         se = delta sx ro
                         b = -2.0 * vectorsum (rd * se)
                         a = vectorsum (rd * rd)
                         c = vectorsum (se * se) - (r * r)
