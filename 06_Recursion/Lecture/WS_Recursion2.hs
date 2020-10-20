{- Arbeitsblatt: Recursion II -}

{-
Gesucht sind jeweils eigene rekursive Definitionen, ohne die Verwendung
von Standardfunktionen.

Falls Sie die Aufgaben einfach finden, implementieren Sie die folgenden
Funktionen komplett, d.h. überprüfen Sie auch die übergebenen Parameter
ob sie überhaupt sinnvoll sind. Sie können mit error „eine Fehlermeldung“
eine Meldung auf der Konsole ausgeben und das Programm abbrechen.
-}

-- Aufgabe 1
-- Implementieren Sie eine Funktion welche die Länge einer Liste bestimmt:

len :: [a] -> Int
len [] = 0
len (_ : xs) = 1 + len xs

-- Aufgabe 2
-- Implementieren Sie eine Funktion, welche überprüft ob in einer Liste von
-- Bool alle Werte True sind.
-- Hinweis: allTrue [] soll auf True evaluieren.
--Variante 1:
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (True : bs) = allTrue bs
allTrue (False : _) = False

--Variante 2:
allTrue' :: [Bool] -> Bool
allTrue' [] = True
allTrue' (b : bs) = b && allTrue bs

-- Aufgabe 3
-- Implementieren Sie eine Funktion, welche aus einer List eine Teilliste extrahiert,
-- indem dessen Startposition und Länge gegeben ist.

-- Beispiel: sublist 3 7 "Hello World" == "lo Worl"
--Lösung Pascal

sublist :: Int -> Int -> [a] -> [a]
sublist _ _ [] = error "Leere Liste übergeben"
sublist _ 0 [] = []
sublist 0 0 _ = []
sublist f s l = secondPart s (firstPart f l) []

--Helper for First Part
firstPart :: Int -> [a] -> [a]
firstPart 0 l = l
firstPart b (_ : xs) = firstPart (b -1) xs

--Helper for Second Part
secondPart :: Int -> [a] -> [a] -> [a]
secondPart 0 _ f = f
secondPart s (x : xs) f = secondPart (s -1) xs (f ++ [x])

-- /////////////////////////////
--Lösung Kröni
sublistK :: Int -> Int -> [a] -> [a]
sublistK _ _ [] = error "Leere Liste übergeben"
sublistK _ 0 [] = []
sublistK 0 l (x : xs) = x : sublistK 0 (l -1) xs    -- Liste von X anhängen       --> gleich wie First Part bei Lösung Pascal
sublistK s l (_ : xs) = sublistK (s -1) l xs        -- Kopfelemente Wegschmeissen --> gleich wie Second Part bei Lösung Pascal

-- Aufgabe 4
-- Implementieren Sie eine Funktion, welche eine Liste an eine andere Liste anhängt:

(+++) :: [a] -> [a] -> [a]
(+++) f s = concatHelper f s []
  where
    concatHelper [] [] r = r
    concatHelper (x : xs) s r = concatHelper xs s (r ++ [x])
    concatHelper [] (x : xs) r = concatHelper [] xs (r ++ [x])

todo :: a
todo = error "todo"
