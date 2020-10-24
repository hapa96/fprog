{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
import System.Environment (withArgs)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- Don't forget to add a type signature (:: Diagram B) to your creation
main = withArgs (words "-o diagram.svg -w 400") (defaultMain diag)
  where diag = modernArt 10 :: Diagram B

atopExample, besideExample, aboveExample :: Diagram B
atopExample   = fc black (square 1) `atop` circle 1
besideExample = fc black (square 1) ||| circle 1
aboveExample  = fc black (square 1) === circle 1

--3.A 
triangle' :: Double -> Diagram B
triangle' 1 = eqTriangle 1
triangle' n = eqTriangle n `atop` triangle' (n-1)

--3.B
drawCircle :: Double -> Diagram B
drawCircle n = cir n True
              where cir :: Double -> Bool -> Diagram B
                    cir 0 _ = mempty --BaseCase
                    cir n c = (cir (n-1) (not c)) `atop` fc (color c) (circle n) --Recursion

--Helper Methode to determine the colour of a Number
color True = black
color False = white

--3.C Modern Art
modernArt  :: Double -> Diagram B
modernArt n = cir n True
            where cir :: Double -> Bool -> Diagram B
                  cir 0 _ = mempty --BaseCase
                  cir n c = (rotateBy(1/6) (cir (n-1) (not c))) `atop` fc (color c)  (square n) --Recursion

--Own Pictures
