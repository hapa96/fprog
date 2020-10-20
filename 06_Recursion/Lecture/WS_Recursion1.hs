--Recursion WS 1
--Aufgabe 1:
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)

--Aufgabe 2:
countDown :: Int -> [Int]
countDown 0 = [0]
countDown n = n : countDown (n -1)

--Aufgabe 3 Count Up
countUp :: Int -> [Int]
countUp 0 = [0]
countUp n = countUp (n-1) ++ [n]

--Aufgabe 4 CountDownUp
countDownUp :: Int -> [Int]
countDownUp 0 = [0]
countDownUp n = n : countDownUp(n-1) ++ [n]
