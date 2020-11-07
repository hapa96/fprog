--1a) safeHead
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

--1b) safeTail
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

--2c) safeMax
safeMax :: (Ord a) => [a] -> Maybe a
safeMax [] = Nothing
safeMax [a] = Just a
safeMax (a:b:as) | a > b  = safeMax (a:as)
                 | otherwise = safeMax (b:as)