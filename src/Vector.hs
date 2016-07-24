module Vector where

import Control.Lens

type Scalar = Double

data Vector = Vector {
  x :: !Scalar,
  y :: !Scalar,
  z :: !Scalar
  }


vector :: Iso' Vector (Scalar, Scalar, Scalar)
vector = iso ( \(Vector x y z) -> (x, y, z))
  (\ (x, y, z) -> Vector x y z)

instance Num Vector where
  (+) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2)
                                        (y1 + y2)
                                        (z1 + z2)
  (-) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 - x2)
                                        (y1 - y2)
                                        (z1 - z2)
  (*) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 * x2)
                                        (y1 * y2)
                                        (z1 * z2)
  negate (Vector x y z) = Vector (0 - x) (0 - y) (0 - z)
  fromInteger x = error "failed to convert integer to vector"

instance Eq Vector where
  (==) (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

instance Show Vector where show (Vector x y z) = show (x, y, z)

mult :: Scalar -> Vector -> Vector
mult s (Vector x y z) = Vector (x * s) (y * s) (z * s)

vectorsum :: Vector -> Scalar
vectorsum (Vector x y z) = x + y + z

normalize :: Vector -> Vector
normalize v@(Vector x y z) = Vector (x / mag) (y / mag) (z / mag)
  where
    mag = sqrt $ dot v v

dot :: Vector -> Vector -> Scalar
dot xs ys = vectorsum (xs * ys)
