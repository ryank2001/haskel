module RSA where
import Data.Char 


-- formule om het ggd te berekenen waarin a het grootste getal is (als dit bij invoer niet zo is wordt dit na de eerste uitvoer omgedraait)
--hierna wordt b net zo vaak van a afgehaald tot b kleiner is dan a, als het getal wat overblijft 0 is is de ggd berekend. zo niet wordt de functie opnieuw aangeroepen met b als a en het getal wat overblijft als b
ggd :: Integral a => a -> a -> a
ggd a b | b == 0 = a
        | otherwise = ggd b (a `mod` b)

--formule die controleerd of een getal is door het proberen te delen door alle getallen die onder het getal zitten.
isPrime :: Integral a => a -> Bool
isPrime number | number < 1 = False
               | otherwise = isPrimeNumberChecker number (number-1)

--extensie van de isPrime functie die ook het getal neemt waarom die op dat moment aan het checken is zodat deze verlaagt kan worden
isPrimeNumberChecker :: Integral a => a -> a -> Bool
isPrimeNumberChecker n 0 = False
isPrimeNumberChecker n m | m == 1 = True
                         | n `mod` m == 0 = False
                         | otherwise = isPrimeNumberChecker n (m-1)

--functie die het dichtsbijzijnde hogere priemgetal zoekt door vanaf n 1 voor 1 alle getallen te checken of deze priem zijn tot er een priemgetal gevonden is
findClosestPrimeHigher :: Integral a => a -> a
findClosestPrimeHigher number | isPrime number = number
                              | otherwise = findClosestPrimeHigher (number+1)


--function to make sure that if the 2 primes are the same, it will find a new  higher prime for the second input and return this
notTheSamePrime :: Integral a => a -> a -> a
notTheSamePrime number1 number2 | number1 == number2 = findClosestPrimeHigher (number2+1)
                                | otherwise = number2


--i dunno what is happening here but it works
extendedGCD :: Integral a => (a,a) -> (a, a)
extendedGCD (a,b)
        |b == 0 = (1,0)
        |otherwise = (t, z)
        where (s,t) = extendedGCD(b, a `mod` b)
              z = s - ( (a `div` b) * t)

--functie die het laagst mogelijke openbare exponent zoekt. Omdat dit exponent niet deelpaar mag zijn door de indicator van de modulus wordt er gekeken of de ggd van de indicator en de exponent 1 is. 
--zo niet wordt de functie opnieuw aangeroepen met de exponent +1 tot de ggd 1 is
openbaarExponent :: Integral a => a -> a -> a
openbaarExponent  indicator waarde | ggd indicator waarde == 1 = waarde
                               | otherwise = openbaarExponent indicator (waarde+1)


--functie met 2 intergral inputs. De inputs zijn 2 willekeurige getallen waarvan de dichtsbijzijnde hogere priemgetallen worden gezocht.
--met deze priemgetallen wordt de modulus berekend en de indicator. De indicator wordt gebruikt om het openbare exponent te berekenen.
--het openbare exponent wordt gebruikt om het private exponent te berekenen.
createKeys :: Integral a => a -> a -> (a, a, a)
createKeys number1 number2 = (modules, public, privateExponent)
    where priem1 = findClosestPrimeHigher number1
          priem2 = notTheSamePrime priem1 (findClosestPrimeHigher number2)
          modules = priem1 * priem2
          indicator = (priem1 - 1) * (priem2 - 1)
          public = openbaarExponent indicator 2
          (private,_) = extendedGCD (public, indicator)
          privateExponent = if private < 0 then private + indicator else private


--functie voor het encrypten en decrypten van een getal. De functie heeft 2 inputs. De eerste input is een tuple met daarin de modulus en het exponent.
rsaEncrypt :: Integral a => (a,a) -> a -> a
rsaEncrypt (modules, exponent) bericht=   bericht ^ exponent `mod` modules

--functie die een string omzet naar een lijst van getallen. Deze getallen worden vervolgens geencrypt met de rsaEncrypt functie
rsaEncryptString :: Integral a => (a,a) -> String -> [a]
rsaEncryptString (modules, exponent) bericht = [rsaEncrypt (modules, exponent) (fromIntegral (ord x)) | x <- bericht]

--functie die een lijst van getallen decrypt met de rsaEncrypt functie
rsaDecryptString :: Integral a => (a,a) -> [a] -> String
rsaDecryptString (modules, exponent) bericht = [chr (fromIntegral (rsaEncrypt (modules, exponent) x)) | x <- bericht]


          









