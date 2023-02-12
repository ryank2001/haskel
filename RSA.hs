module RSA where
import Data.Char 

ggd :: Integral a => a -> a -> a
ggd a b | b == 0 = a
        | otherwise = ggd b (a `mod` b)

isPrime :: Integral a => a -> Bool
isPrime number | number < 1 = False
               | otherwise = isPrimeNumberChecker number (number-1)

isPrimeNumberChecker :: Integral a => a -> a -> Bool
isPrimeNumberChecker n 0 = False
isPrimeNumberChecker n m | m == 1 = True
                         | n `mod` m == 0 = False
                         | otherwise = isPrimeNumberChecker n (m-1)

findClosestPrimeHigher :: Integral a => a -> a
findClosestPrimeHigher number | isPrime number = number
                              | otherwise = findClosestPrimeHigher (number+1)


--function to make sure that if the 2 primes are the same, it will find a new  higher prime for the second input and return this
notTheSamePrime :: Integral a => a -> a -> a
notTheSamePrime number1 number2 | number1 == number2 = findClosestPrimeHigher (number2+1)
                                | otherwise = number2


extendedGCD :: Integral a => (a,a) -> (a, a)
extendedGCD (a,b)
        |b == 0 = (1,0)
        |otherwise = (t, z)
        where (s,t) = extendedGCD(b, a `mod` b)
              z = s - ( (a `div` b) * t)

openbaarExponent :: Integral a => a -> a -> a
openbaarExponent  euler waarde | ggd euler waarde == 1 = waarde
                               | otherwise = openbaarExponent euler (waarde+1)

createKeys :: Integral a => a -> a -> (a, a, a)
createKeys number1 number2 = (modules, public, privateExponent)
    where priem1 = findClosestPrimeHigher number1
          priem2 = notTheSamePrime priem1 (findClosestPrimeHigher number2)
          modules = priem1 * priem2
          euler = (priem1 - 1) * (priem2 - 1)
          public = openbaarExponent euler 2
          (private,_) = extendedGCD (public, euler)
          privateExponent = if private < 0 then private + euler else private



rsaEncrypt :: Integral a => (a,a) -> a -> a
rsaEncrypt (modules, exponent) bericht=   bericht ^ exponent `mod` modules

rsaDecrypt :: Integral a => (a,a) -> a -> a
rsaDecrypt (modules, exponent) bericht=   bericht ^ exponent `mod` modules

rsaEncryptString :: Integral a => (a,a) -> String -> [a]
rsaEncryptString (modules, exponent) bericht = [rsaEncrypt (modules, exponent) (fromIntegral (ord x)) | x <- bericht]

rsaDecryptString :: Integral a => (a,a) -> [a] -> String
rsaDecryptString (modules, exponent) bericht = [chr (fromIntegral (rsaDecrypt (modules, exponent) x)) | x <- bericht]


          









