module PokerSpel where

import Data.List
import Data.Char
import Distribution.Simple.Setup (falseArg)




--functie die checked hoeveel verschillende getallen er dubbel zijn in de lijst
--dit doet deze door voor elke unieke char in de lijst checkdouble te roepen en te kijken of er meer dan 1 van is vervolgens wordt de lengte van de lijst met de unieke chars terug gegeven
dubbelen :: [Int] -> Int
dubbelen [] = 0
dubbelen list = length([x | x <- nub list,length (checkDouble list x) >1 ])

--Functie die een lijst met ints checked op meegereven int en hoe vaak deze voor komt
--De functie loopt door de lijst met ints heen en voor elke int wordt deze vergeleken met de meegegeven int. Als de ints gelijk zijn wordt er een bool in de lijst gezet
--de lengte van de lijst geeft dus de hoeveelheid dat het getal voorkomt
checkDouble :: [Int] -> Int-> [Bool]
checkDouble list num = [True | x <- list , x == num]

--functie die een lijst met int checked op het getal dat het vaakst voorkomt en hoe vaak
--de functie werkt door voor elke unieke int in de lijst checkdouble te roepen en te kijken hoe vaak deze voorkomt.
--hierna wordt het maximum gepakt. Dus het getal dat het vaakst voorkomt
checkThrowForBiggestDuplicate :: [Int] -> Int
checkThrowForBiggestDuplicate list = maximum([length (checkDouble list x) | x <- nub list])

checkstraight :: [Int] -> Bool
checkstraight list | 1 `elem` list && 6 `elem` list  = False
                   | otherwise = True
                

--functie die een lijst met 5 ints checked op de verschillende poker hands
--de functie werkt door eerst te kijken hoe vaak het grootste getal voorkomt in de lijst. Dit wordt gedaan door checkThrowForBiggestDuplicate te gebruiken
--Vervolgens wordt er gekeken hoe vaak er dubbele getallen voorkomen in de lijst. Dit wordt gedaan door de dubbelen functie te gebruiken
--Vervolgens wordt er gekeken welke hand het is en wordt er een string terug gegeven

checkThrow :: [Int] -> [Char]
checkThrow throw | biggestDupe == 1 =  if checkstraight throw then "Straight" else "bust"
                 | biggestDupe == 2  = if double == 2 then "Two pair" else "One pair"
                 | biggestDupe == 3  = if double == 2 then "Full House" else "Three of a kind"
                 | biggestDupe == 4  = "Four of a kind"
                 | biggestDupe == 5  = "Poker"
               where biggestDupe = checkThrowForBiggestDuplicate throw
                     double = dubbelen throw

--Functie die de functie checkThrow aanroept voor elke mogelijke worp en de kans berekent dat deze worp voorkomt
checkOdds ::  [([Char], Float)] 
checkOdds = let poker = throwchance "Poker"
                fourOfAKind = throwchance "Four of a kind"
                fullHouse = throwchance "Full House"
                straight = throwchance "Straight"
                threeOfAKind = throwchance "Three of a kind"
                twoPair = throwchance "Two pair"
                onePair = throwchance "One pair"
                bust = throwchance "bust"
            in [("poker", poker), ("four of a kind", fourOfAKind), ("full house", fullHouse), ("straight", straight), ("three of a kind", threeOfAKind), ("two pair", twoPair), ("one pair", onePair), ("bust", bust)]
            where allThrows = [checkThrow[t1,t2,t3,t4,t5] | t1 <- [1..6], t2 <- [1..6], t3 <- [1..6], t4 <- [1..6], t5 <- [1..6]]
                  totalThrows = 6^5
                  throwchance a = fromIntegral ( length (filter (== a) allThrows)) / totalThrows *100


