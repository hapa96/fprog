-- | The Calculation Module includes different Calculation Methods to calculate all kind of things.
module HGrade.Calculation where

import Data.List (sort)

-- | Median
-- | Calculates the Median for a Topic. Takes a Int-List and returns a Double value, which stands for the Median. The Int-List represents all Grades for a single Topic.
-- | If an empty List is passed to the function, return -1.0, this indicates an invalid input
median :: [Int] -> Double
median [] = -1.0
median list
  | even $ length list = ((intToDouble list !! ((length list `div` 2) -1)) + (intToDouble list !! (length list `div` 2))) / 2
  | otherwise = intToDouble list !! (length list `div` 2)

-- | Build the Medians for an entire Site. Takes a List of Int-Lists and returns a List of Doubles. Each Int-List represents Grades for a Topic.
buildAllMedians :: [[Int]] -> [Double]
buildAllMedians = map (median . sort)

-- | Histogram
-- | Calculate the Values for the Histogram. Takes a List of Integers, which represents the Grades for a Topic and returns a Int Tripple, which describes the occurrence of the different grades => (0,1,2)
-- | Empty List returns Tripple (-1,-1,-1). This indicates an invalid input
histogram :: [Int] -> (Int, Int, Int)
histogram [] = (-1, -1, -1)
histogram list = convertArrayToTripple $ countOccurrence list [0, 0, 0]

-- | Build all the Histograms for an entire Site. Takes a List of integerLists and returns a List of Int-Tripples
buildAllHistograms :: [[Int]] -> [(Int, Int, Int)]
buildAllHistograms = map histogram

-- | Helper Method for Histogram Calculation. Counts the occurence of the grades and returns a corresponding Array
countOccurrence :: [Int] -> [Int] -> [Int]
countOccurrence [] res = res
countOccurrence (x : xs) (zero : one : two : [])
  | x == 0 = countOccurrence xs $ zero + 1 : one : two : []
  | x == 1 = countOccurrence xs $ zero : one + 1 : two : []
  | otherwise = countOccurrence xs $ zero : one : two + 1 : []

-- | Helper Methods for different Tasks
-- | Takes a Int Array and returns a Int Tripple
convertArrayToTripple :: [Int] -> (Int, Int, Int)
convertArrayToTripple (x : y : z : []) = (x, y, z)

-- | Converts a List of Integers into List of Doubles
intToDouble :: [Int] -> [Double]
intToDouble = map fromIntegral

-- | Transforms Colums to Rows for Calculation of the Median
colsToRows :: [[a]] -> [[a]]
colsToRows ([] : _) = []
colsToRows x = map head x : colsToRows (map tail x)