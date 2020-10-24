--Insertion Sort Algorithmus

--Sortiert eine beliebige Liste
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

-- Fügt ein Element in eine Liste am richtigen Platz ein
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x : xs)
  | a <= x = (a : x : xs)
  | otherwise = x : insert a xs