colsToRows :: [[a]] -> [[a]]
colsToRows ([] : _) = []
colsToRows x = map head x : colsToRows (map tail x)