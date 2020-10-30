{- HTest - Tiny Haskell Testing Framework -}

-- a)
length' :: [a] -> Int
length' [] = 0
length' (_ : as) = 1 + length' as

test1 :: Eq o => (i -> o) -> [(i, o)] -> Bool
test1 f ios = and (map (\(i, o) -> f i == o) ios)

lengthTestData :: [([Int], Int)]
lengthTestData =
  [ ([], 0),
    ([0], 1),
    ([0, 1], 2)
  ]

lengthTest :: Bool
lengthTest = test1 length' lengthTestData

-- b)
test2 :: Eq o => (i1 -> i2 -> o) -> [((i1, i2), o)] -> Bool
test2 f ios = and (map (\((i1, i2), o) -> f i1 i2 == o) ios)

dropTestData :: [((Int, [Int]), [Int])]
dropTestData =
  [ ((0, []), []),
    ((1, []), []),
    ((1, [0]), []),
    ((1, [0, 1]), [1])
  ]

dropTest :: Bool
dropTest = test2 drop dropTestData

--1 Recursion
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' n (_ : xs) = drop' (n -1) xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x : xs) = [x] ++ take' (n -1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' (_:_) [] = []
zip' [] (_:_) = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) | e == x = True
               | otherwise = elem' e xs

eqList' :: Eq a => [a] -> [a] -> Bool
eqList' [] [] = True
eqList' (_:_) [] = False
eqList' [] (_:_) = False
eqList' (x:xs) (y:ys) | x == y = eqList' xs ys
                      | otherwise = False


