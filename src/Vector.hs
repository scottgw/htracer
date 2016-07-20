module Vector where

import Data.Tuple.Select

type Scalar = Double

newtype Vector = Vector (Scalar, Scalar, Scalar)

instance Num Vector where
  (+) (Vector xs) (Vector ys) = Vector (sel1 xs + sel1 ys,
                                        sel2 xs + sel2 ys,
                                        sel3 xs + sel3 ys)
  (-) (Vector xs) (Vector ys) = Vector (sel1 xs - sel1 ys,
                                        sel2 xs - sel2 ys,
                                        sel3 xs - sel3 ys)
  (*) (Vector xs) (Vector ys) = Vector (sel1 xs * sel1 ys,
                                        sel2 xs * sel2 ys,
                                        sel3 xs * sel3 ys)
  negate (Vector xs) = Vector (0 - sel1 xs, 0 - sel2 xs, 0 - sel3 xs)
  fromInteger x = error "failed to convert integer to vector"

instance Eq Vector where
  (==) (Vector xs) (Vector ys) = all id [sel1 xs == sel1 ys,
                                         sel2 xs == sel2 ys,
                                         sel3 xs == sel3 ys]

instance Show Vector where show (Vector xs) = show xs

mult :: Scalar -> Vector -> Vector
mult s (Vector xs) = Vector (sel1 xs * s, sel2 xs * s, sel3 xs * s)

vectorsum :: Vector -> Scalar
vectorsum (Vector xs) = sel1 xs + sel2 xs + sel3 xs

normalize :: Vector -> Vector
normalize v@(Vector vs) = Vector (sel1 vs / mag, sel2 vs / mag, sel3 vs / mag)
  where
    mag = sqrt $ dot v v

dot :: Vector -> Vector -> Scalar
dot xs ys = vectorsum (xs * ys)
