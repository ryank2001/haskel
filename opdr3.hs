module Opdr3 where

import Data.List
import Data.Char


-- test functie voor de diffentieer functie
functie :: Double -> Double
functie x = x^2 +5 

-- test functie voor de diffentieer functie
functie2 :: Double -> Double
functie2 x = x*2 +5

-- test functie voor de diffentieer functie
functie3 :: Double -> Double
functie3 x = 3

--functie voor het differentieren van een functie
differentieer :: (Double -> Double) -> Double ->  Double -> Double
differentieer f x p= (f (x+p)-f x)/p


intergreer :: (Double -> Double) -> Double -> Double -> Double -> Double
intergreer f a b p = sum [(f x)*p | x <- [a, a+p ..b]]

--functie die een [char] checked op een megegeven char. Voor elke van de chars in de lijst wordt er een bool in een lijst gezet
checkCharacter :: [Char] -> Char-> [Bool]
checkCharacter list char = [True | x <- list , x == char]

--Functie die een lijst met chars checked op dubbele chars
--werkt door alle unieke characters te checken (unieke characters worden verkregen met nub functie) en te kijken of er meer dan 1 van is door de checkCharacter functie te gebruiken
dubbelen :: [Char] -> [Char]
dubbelen [] = []
dubbelen list = [x | x <- nub list,length (checkCharacter list x) >1 ]

--\\  //
