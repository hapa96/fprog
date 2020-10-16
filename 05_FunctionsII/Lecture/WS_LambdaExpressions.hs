incAll :: [Int] -> [Int]
incAll = map(\x -> x+1)

addToAll :: Int -> [Int] -> [Int]
addToAll a = map(\x -> x+ a)
-- filter behält alles, was zu true evaluiert.
keepOld :: [Int] -> [Int]
keepOld = filter(\x -> x >= 90)

dropShort :: [String] -> [String]
dropShort = filter(\x ->length x > 1 )

