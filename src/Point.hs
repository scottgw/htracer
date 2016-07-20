module Point where

import Data.Tuple.Select
import Vector

newtype Point = Point (Scalar, Scalar, Scalar)

delta :: Point -> Point -> Vector
delta (Point p) (Point q) = Vector(sel1 p - sel1 q,
                                   sel2 p - sel2 q,
                                   sel3 p - sel3 q)

translate :: Point -> Vector -> Point
translate (Point p) (Vector v) = Point (sel1 p + sel1 v,
                                        sel2 p + sel2 v,
                                        sel3 p + sel3 v)

instance Eq Point where
  (==) (Point p) (Point q) = all id [sel1 p == sel1 q,
                                     sel2 p == sel2 q,
                                     sel3 p == sel3 q]

instance Show Point where show (Point p) = show p

origin = Point (0,0,0)
