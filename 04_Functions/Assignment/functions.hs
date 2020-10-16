-- Übung 04: Functions
--Aufgabe 01: Implementation der  Compare-Funktion
--a)
compare_if_then_else :: (Ord a) => a -> a -> Ordering
compare_if_then_else a b =
  if a > b
    then GT
    else
      if b > a
        then LT
        else EQ

--b)
compare_guards :: (Ord a) => a -> a -> Ordering
compare_guards a b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

--c)
compare_case_expression :: (Ord a) => a -> a -> Ordering
compare_case_expression a b = case a > b of
  True -> GT
  False -> case a == b of
    True -> EQ
    False -> LT

--d)
-- Die Fallunterschiedung ist mit einem Patternmatching nicht zu realisieren, da ein Vergleich notwendig ist. Selbst die (verrückte) Idee, alle möglichen Fälle als Pattern anzugebe, ist in diesem Falle nicht mehr möglich, 
-- da man über die Werte vom Typ a nur weiss, dass sie vergleichbar sind (Ord a). Man kan kein Pattern schreiben, welche für WErte der unterschiedlichen Typen gleichzeitig passen. Zudem gibt es Typen (Int) welche Unentlich viele Möglichkeiten haben
-- ######################################################
--2) Siehe OneNote

--3) Matrix Libary
--Definition 2x2- Matrix
type M22 = ((Int, Int), (Int, Int))

add :: M22 -> M22 -> M22
add ((a11, a12), (a21, a22)) ((b11, b12), (b21, b22)) = ((a11 + b11, a12 + b12), (a21 + b21, a22 + b22))

sub :: M22 -> M22 -> M22
sub ((a11, a12), (a21, a22)) ((b11, b12), (b21, b22)) = ((a11 - b11, a12 - b12), (a21 - b21, a22 - b22))

mulS :: M22 -> Int -> M22
mulS ((a11, a12), (a21, a22)) s = ((a11 * s, a12 * s), (a21 * s, a22 * s))

mul :: M22 -> M22 -> M22
mul ((a11, a12), (a21, a22)) ((b11, b12), (b21, b22)) = ((a11 * b11 + a12 * b21, a11 * b12 + a12 * b22), (a21 * b11 + a22 * b21, a21 * b12 + a22 * b22))