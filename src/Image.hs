{-# LANGUAGE OverloadedStrings #-}
module Image (RGB(..), Image(..), write_ppm) where

import Data.List
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BLB

data RGB = RGB {r :: !Int,
                g :: !Int,
                b :: !Int}

instance Show RGB where
  show = BL.unpack . BLB.toLazyByteString . rgb_to_str

instance Num RGB where
  (+) (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

intByteString :: Int -> BL.ByteString
intByteString = BLB.toLazyByteString . BLB.intDec

-- Convert an RGB to a String
rgb_to_str :: RGB -> BLB.Builder
rgb_to_str (RGB r g b) = BLB.intDec r <> " " <> BLB.intDec g <> " " <> BLB.intDec b

-- Test for rgb_to_str
test_rgb_to_str :: Bool
test_rgb_to_str = BLB.toLazyByteString (rgb_to_str (RGB 11 12 13)) == "11 12 13"

newtype Image = Image (Int, Int, [RGB])

-- Create a ppm file.
create_ppm :: Image -> BL.ByteString
create_ppm (Image (w, h, ps)) = BL.unlines line_list
  where
    rgbs = BLB.toLazyByteString
      (mconcat (intersperse (BLB.char8 '\n') (map rgb_to_str ps)))

    line_list :: [BL.ByteString]
    line_list = ["P3", BL.unwords [intByteString w, intByteString h], "255", rgbs]

write_ppm :: FilePath -> Image -> IO ()
write_ppm file = BL.writeFile file . create_ppm

-- -- Test for create_ppm
test_create_ppm :: Bool
test_create_ppm = create_ppm (Image (2, 3, [RGB 11 12 13, RGB 22 23 24,
                                            RGB 33 34 35, RGB 44 45 46,
                                            RGB 55 56 57, RGB 66 67 68])) ==
                  "P3\n2 3\n255\n11 12 13\n22 23 24\n33 34 35\n44 45 46\n55 56 57\n66 67 68\n"

test_write_ppm = write_ppm "test.ppm" (Image (16, 16, (map mkrgb [0..255])))
  where
    mkrgb x = RGB x x x
