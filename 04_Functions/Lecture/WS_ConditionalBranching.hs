maxGuards' :: Int -> Int -> Int
--Mit Guards
maxGuards' a b  | a > b = a
                | otherwise = b -- Otherwise is defined as True

-- If-then-Else Expression
maxIfElse' :: Int -> Int -> Int
maxIfElse' a b = if a > b
                then a
                else  b


-- Case Expression
maxCase' :: Int -> Int -> Int
maxCase' a b = case a > b of
                True -> a
                False -> b
