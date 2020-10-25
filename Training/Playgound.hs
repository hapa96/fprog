module Rot13 where
-- import Data.Char (ord)
import Data.Char

rot13 :: String -> String
rot13 [x] =  [rot13Transformation (ord x)]--BaseCase
rot13 (x:xs) = [rot13Transformation (ord x)] ++ rot13 xs  --Recursion


rot13Transformation :: Int -> Int
rot13Transformation n |  n > 64 &&  n < 91 = calculateNumber (n-65) + 65  --LowerCase Letter
                      |  n > 96 &&  n < 123 = calculateNumber (n-97) + 97 --UpperCase Letter
                      | otherwise =  (chr n)

calculateNumber :: Int -> Int
calculateNumber n = ((n+13) `mod` 27)    