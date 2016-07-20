module Image where

-- Join a list of strings using a separator.
join :: String -> [String] -> String
join sep [x] = x
join sep (x:xs) = x ++ sep ++ join sep xs

-- Test for join
test_join :: Bool
test_join = join "t" ["12", "3", "", "xy"] == "12t3ttxy"

newtype RGB = RGB (Int, Int, Int)

instance Show RGB where show rgb = show $ rgb_to_str rgb

-- Convert an RGB to a String
rgb_to_str :: RGB -> String
rgb_to_str (RGB (r,g,b)) = join " " $ map show [r,g,b]

-- Test for rgb_to_str
test_rgb_to_str :: Bool
test_rgb_to_str = rgb_to_str (RGB (11,12,13)) == "11 12 13"

newtype Image = Image (Int, Int, [RGB])

-- Create a ppm file.
create_ppm :: Image -> String
create_ppm (Image (w, h, ps)) = join "\n" line_list ++ "\n"
  where
    line_list = ["P3", show w ++ " " ++ show h, "255"] ++ map rgb_to_str ps

-- -- Test for create_ppm
test_create_ppm :: Bool
test_create_ppm = create_ppm (Image (2, 3, [RGB (11,12,13), RGB (22,23,24),
                                            RGB (33,34,35), RGB (44,45,46),
                                            RGB (55,56,57), RGB (66,67,68)])) ==
                  "P3\n2 3\n255\n11 12 13\n22 23 24\n33 34 35\n44 45 46\n55 56 57\n66 67 68\n"

test_write_ppm = writeFile "test.ppm" $ create_ppm (Image (16, 16, (map mkrgb [0..255])))
  where
    mkrgb x = RGB(x,x,x)
