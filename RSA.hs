module RSA where
import Data.Char 


extendedGCD :: Integral a => (a,a) -> (a, a)
extendedGCD (a,b)
        |(b == 0) = (1,0)
        |otherwise = (t, z)
        where (s,t) = extendedGCD(b, a `mod` b)
              z = s - ( (a `div` b) * t)

createKeys :: Integral a => a -> a -> (a, a, a)
createKeys priem1 priem2 = (modules, openbaarExponent, privateExponent)
    where modules = priem1 * priem2
          euler = (priem1 - 1) * (priem2 - 1)
          openbaarExponent = 3
          (private,_) = extendedGCD (openbaarExponent, euler)
          privateExponent = if private < 0 then private + euler else private

rsaEncrypt :: Integral a => (a,a) -> a -> a
rsaEncrypt (euler, exponent) bericht=   bericht ^ exponent `mod` euler

rsaDecrypt :: Integral a => (a,a) -> a -> a
rsaDecrypt (euler, exponent) bericht=   bericht ^ exponent `mod` euler

rsaEncryptString :: Integral a => (a,a) -> String -> [a]
rsaEncryptString (euler, exponent) bericht = [rsaEncrypt (euler, exponent) (fromIntegral (ord x)) | x <- bericht]

rsaDecryptString :: Integral a => (a,a) -> [a] -> String
rsaDecryptString (euler, exponent) bericht = [chr (fromIntegral (rsaDecrypt (euler, exponent) x)) | x <- bericht]


          









