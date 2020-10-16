--Aufgabe 2: Currying
--a) curry'
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

--b) uncurried
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

--Aufgabe 3: flip'
--a
flip' :: (a -> b -> c) -> (b -> a -> c)
--b) Mit Lambda
flip' f = \a b -> f b a
--b1)Parameter Definitionssyntax
--flip' f x y = f y x