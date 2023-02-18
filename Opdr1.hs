
module Opdr1 where
import Data.Bits

-- We declare the name of our module so 
-- it can be imported by name in a project.






--faculteit berekenen met pattern matching
fac1 :: Int -> Int
fac1 0 = 1
fac1 n = product [1..n]


--faculteit berekenen met guards
fac2 :: Int -> Int
fac2 n 
    | n == 0 = 1
    | n /= 0 = product [1..n]



--ABC formule met een where statment
abc :: Double -> Double -> Double -> [Maybe Double]
abc a b c =
    let x1 | d < 0 = Nothing
           |  d >= 0 = Just ((-b + sqrt d) / (2*a))
        x2 | d <= 0 = Nothing
           |  d > 0 = Just ((-b - sqrt d) / (2*a))
    in [x1,x2]
    where d = b^2 - 4*a*c


gooi3dobbel :: [(Int,Int,Int)]
gooi3dobbel = [(x,y,z) | x <- [1..6], y <- [1..6], z <- [1..6],  mod (x+y+z) 5 == 0]

--enkele oplossing is (0,0,0)
--werkt door door een 3 dementionale lijst te maken en deze te filteren op de voorwaarden
opdracht3 :: [(Int,Int,Int)]
opdracht3 = [(x,y,z) | x <- [0..100], y <- [0..100], z <- [0..100],  x == abs(z-y)*2 && y == x*z && z == div (x+y) 2 ]


-- werkt door een list te vullen met getal1 x het getal2 en deze op te tellen
mult :: Integer->Integer->Integer
mult getal1 getal2 = sum [getal1 | x <- [1..getal2]]


binaryNumberCheck :: Integer -> Integer -> Integer
binaryNumberCheck shiftGetal huidigeWaarde 
                    | shiftGetal == 0 = huidigeWaarde
                    | otherwise = binaryNumberCheck (shiftR shiftGetal 1) (huidigeWaarde + 1)

--werkt door het binaire getal 2 te checken op welke posities allemaal 1 staan.
--als dit het geval is wordt getal1 gebitshifted naar links voor de hoeveelheid binaire getallen er achter de 1 stonden en wordt deze in eeen list gezet en opgeteld
--bijv 101 * 101 (5x5) = 10100 (101 bitshifts naar links 2x) + 101 (101 bitshifts naar links 0x) = 11001 (25)
fastmult :: Integer->Integer->Integer
fastmult getal1 getal2 = sum[shiftL getal1 (fromInteger (x-1)) | x <- [1..binaryNumberCheck getal2 1], shiftR getal2 (fromInteger (x-1)) .&. 1== 1]


--werkt door het getal1 keer dezelfde functie te doen met de macht 1 lager tot de macht op 1 staat
pow :: Integer->Integer->Integer
pow 0 macht = 0
pow getal1 0 = 1
pow getal1 1 = getal1
pow getal1 macht = fastmult getal1 (pow getal1 (macht-1))

--pow functie die meer werkt op het concept van een binary tree.
--als de macht een even getal is wordt de macht gehalveerd en wordt de functie 2x aangeroepen
--als de macht een oneven getal is wordt de macht met 1 verlaagd en wordt de functie aangeroepen zoals bij de vorige opgaven
--bijv 2^5 = 2^2 * 2^2 * 2^1
fastpow :: Integer->Integer->Integer
fastpow 0 macht = 0
fastpow getal1 0 = 1
fastpow getal1 1 = getal1
fastpow getal1 macht | even macht = fastmult (fastpow getal1 (shiftR macht 1)) (fastpow getal1 (shiftR macht 1))
                     | odd macht = fastmult getal1 (fastpow getal1 (macht-1))


