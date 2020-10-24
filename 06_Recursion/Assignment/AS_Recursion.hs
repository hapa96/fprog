--Aufgabe1: Maximum
-- #############Lösung Pascal##############
maxl :: (Ord a) => [a] -> a
maxl [] = error "Leere Liste übergeben"
maxl [x] = x
maxl (x : xs) = maxl (max' x (head xs) : tail xs)

--Helper Function für Aufgabe 1. -> Evaluiert von zwei Elementen das grössere und gibt dieses zurück
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b 
-- #############Lösung Kröni##############
maxKröni ::(Ord a) => [a] -> a
maxKröni [] = error "Leere Liste übergeben"
maxKröni [x] = x
maxKröni (x : xs) = max' x (maxKröni xs) 
-----------------------------------------------------------
--Aufgabe 2: reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

--Aufgabe 3: Alternate
alternate :: [Int] -> [Int] -> [Int]
alternate [] bs = bs
alternate (a:as) (bs) = a:(alternate bs as)

--Aufgabe 4 : Fibonacci Recursively
fib :: Int -> Int
fib 0 = error "0 is not a valid Input."
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)