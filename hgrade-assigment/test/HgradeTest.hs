-- | Unit Tests for Hgrade Application
module Main (main) where

import HGrade.Calculation (colsToRows, histogram, median)
import Test.Tasty
import Test.Tasty.HUnit

-- | Entry Points for Unit Tests
main :: IO ()
main = defaultMain tests

-- | Tests all unit-tests, which are referenced within the array
tests :: TestTree
tests = testGroup "Tests" [unitTestsColsToRows, unitTestsMedian, unitTestsHistogram]

-- | Tests functionallity for ColsToRows
unitTestsColsToRows :: TestTree
unitTestsColsToRows =
  testGroup
    "Unit tests for ColsToRows"
    [ testCase "colsToRow with singelton List" $
        colsToRows [[1, 2, 3]] @?= [[1], [2], [3]],
      testCase "colsToRow with larger List" $
        colsToRows [[1, 2, 3], [4, 5, 6], [7, 8, 9]] @?= [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
    ]

-- | Tests functionallity for median
unitTestsMedian :: TestTree
unitTestsMedian =
  testGroup
    "Unit tests for Median"
    [ testCase "empty List" $
        median [] @?= -1.0,
      testCase "singelton List" $
        median [1] @?= 1.0,
      testCase "Odd Input List" $
        median [1, 2, 3, 4, 5, 6, 7] @?= 4.0,
      testCase "Even Input List" $
        median [1, 2, 3, 4, 5, 6] @?= 3.5
    ]

-- | Tests functionallity for Histogram
unitTestsHistogram :: TestTree
unitTestsHistogram =
  testGroup
    "Unit tests for Histogram"
    [ testCase "empty List" $
        histogram [] @?= (-1, -1, -1),
      testCase "singelton List with 0" $
        histogram [0] @?= (1, 0, 0),
      testCase "singelton List with 1" $
        histogram [1] @?= (0, 1, 0),
      testCase "singelton List with 2" $
        histogram [2] @?= (0, 0, 1),
      testCase "larger Input" $
        histogram [1, 2, 2, 0, 1, 0, 2] @?= (2, 2, 3)
    ]
