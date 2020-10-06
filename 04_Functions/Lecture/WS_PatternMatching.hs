--1) Tauscht die ersten beiden Elemente in einer Liste. Wenn Liste < 2 Elemente ist, bleibt sie unverändert
switchFirstTwo:: [a] -> [a]
switchFirstTwo [] = [] --Lere Liste
switchFirstTwo (a:[]) = (a:[]) --Liste hat ein Element
switchFirstTwo(a:b:c) = (b:a:c) --Liste hat zwei oder mehr Elemente --> c kann auch leere Liste sein

--2) Deklariere Typ Vector
type Vec = (Int, Int)

--a) addVec addiert zwei Vektoren
addVec:: Vec -> Vec -> Vec
addVec (ax,ay) (bx,by) = (ax + bx , ay + by)

--b) Optimiere addVec 
addVecOpt :: Vec -> Vec -> Vec
addVecOpt (ax,ay)(bx,by) = (addOpt ax bx, addOpt ay by)

--Hilfsfunktion für addVecOpt --> Addition mit null wird wegoptimiert
addOpt :: Int -> Int -> Int
addOpt 0 n = n
addOpt n 0 = n
addOpt n m = n+m