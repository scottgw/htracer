module Point where

import Control.Lens

import Vector

newtype Point = Point Vector


delta :: Point -> Point -> Vector
delta (Point p) (Point q) = p - q

translate :: Point -> Vector -> Point
translate (Point p) v = Point (p + v)

instance Eq Point where
  (==) (Point p) (Point q) = p == q

instance Show Point where show (Point p) = show p

origin = Point (Vector 0 0 0)
