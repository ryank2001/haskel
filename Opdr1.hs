
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
fac2 n | n == 0 = 1
    | n /= 0 = product [1..n]


--ABC formule met een where statment
abc :: Double -> Double -> Double -> [Double]
abc a b c =
    let x1 | d < 0 = 0
         |  d >= 0 = (-b + sqrt d) / (2*a)
        x2 | d < 0 = 0
         |  d >= 0 = (-b - sqrt d) / (2*a)
    in [x1,x2]
    where d = b^2 - 4*a*c

gooi3dobbel :: [(Int,Int,Int)]
gooi3dobbel = [(x,y,z) | x <- [1..6], y <- [1..6], z <- [1..6],  mod (x+y+z) 5 == 0]

-- enkele oplossing is (0,0,0)
opdracht3 :: [(Int,Int,Int)]
opdracht3 = [(x,y,z) | x <- [0..100], y <- [0..100], z <- [0..100],  x == abs(z-y)*2 && y == x*z && z == div (x+y) 2 ]

-- mult 9999999999999999 9999999999999999999 getallen die stack overflow geven
mult :: Int->Int->Int
mult getal1 getal2 = sum [getal1 | x <- [1..getal2]]

fastmult :: Int->Int->Int
fastmult getal1 getal2 = sum[shiftL getal1 (x-1) | x <- [1..(finiteBitSize getal2 - countLeadingZeros getal2)], shiftR getal2 (x-1) .&. 1== 1]

pow :: Int->Int->Int
pow 0 getal2 = 0
pow getal1 0 = 1
pow getal1 1 = getal1
pow getal1 getal2 = fastmult getal1 (pow getal1 (getal2-1))

fastpow :: Int->Int->Int
fastpow 0 getal2 = 0
fastpow getal1 0 = 1
fastpow getal1 1 = getal1
fastpow getal1 getal2 | even getal2 = fastmult (fastpow getal1 (shiftR getal2 1)) (fastpow getal1 (shiftR getal2 1))
                         | odd getal2 = fastmult getal1 (fastpow getal1 (getal2-1))


