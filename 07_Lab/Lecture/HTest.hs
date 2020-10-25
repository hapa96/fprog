{- HTest - Tiny Haskell Testing Framework -}

-- a)
length' :: [a] -> Int
length' []     = 0
length' (_:as) = 1 + length' as


test1 f ios = todo

lengthTestData :: [([Int], Int)]
lengthTestData = 
  [([],    0),
   ([0],   1),
   ([0,1], 2)]

lengthTest :: Bool
lengthTest = test1 length' lengthTestData


-- b)
test2 f ios = todo

dropTestData :: [((Int, [Int]), [Int])]
dropTestData = 
  [((0, []),    []),
   ((1, []),    []),
   ((1, [0]),   []),
   ((1, [0,1]), [1])]

dropTest :: Bool
dropTest = test2 drop dropTestData


todo = error "todo"