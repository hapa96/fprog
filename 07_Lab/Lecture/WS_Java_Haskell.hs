--a) getGrades
getGrades :: [(a, b)] -> [b]
getGrades = map snd

--b) pow2
pow2 :: [Int] -> [Int]
pow2 = map (\x -> x * x)

--c) adultNames
adultNames :: [(String, Int)] -> [String]
adultNames = map fst . filter (\(_, y) -> y >= 18)