-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  yes
-- License     :  yes
--
-- Maintainer  :  1039779@hr.nl
-- Stability   :  Very very Unstable
-- Portability :  porttable
--
-- This module contains the algorithm to compress a file using huffman compression
--
-----------------------------------------------------------------------------

module Main where
import BinaryTree
import InputValidate
import Data.List (sortBy, sortOn, nub)
import System.Directory.Internal.Prelude (getArgs)
import Data.ByteString (ByteString)
import GHC.Exts.Heap (GenClosure(value))
import Data.Bits (Bits(shift))
import qualified Data.Binary as Binary
import Data.Word
import Data.ByteString as BS





{-
Function 'checkCharacter' checks how many times a character occurs in a String.
Takes 2 arguments:
    'list': the string that is being checked
    'char': the character that is being checked
returns the amount of times the character occurs in the string.
-}
checkCharacter :: [Char] -> Char-> Int
checkCharacter list char =Prelude.length [True | x <- list , x == char]

{-
Function 'dubbelen' checks how many times a character occurs in a String.
Takes 1 argument:
    'list': the string that is being checked
returns a list of tuples with the character and the amount of times it occurs.
-}
doubles :: [Char] -> [(Char,Int)]
doubles [] = []
doubles list = [(x,checkCharacter list x) | x <- nub list]


{-
Function 'maptoTree' converts a list of tuples to a binary tree that holds the characters in the file to be encrypted.
Takes 1 argument:
    'list': the list of tuples that is being converted
returns a binary tree with the characters in the file to be encrypted.
-}
maptoTree :: [(Char,Int)] -> Bintree Char
maptoTree [] = Empty
maptoTree ((x,y):xs) = Node x Empty $ maptoTree xs



{-
Function 'createBinaryTree' creates a binary tree that holds the characters in the file to be encrypted.
Takes 1 argument:
    'input': the content of the file to be encrypted
the return value is a binary tree with the most used characters at the top and the least used characters at the bottom.
-}
createBinaryTree :: String -> Bintree Char
createBinaryTree [] = Empty
createBinaryTree input = maptoTree $ sortBy (flip(\(_,a) (_,b) -> compare a b)) $ doubles input



{-
Function 'addBinOnes' checks in the bintree how many 1's are needed to encrypt a character and adds those.
Takes 3 arguments:
    'tree': the binary tree that holds the characters in the file to be encrypted
    'char': the character that is being encrypted
    'value': the bitstring that has been encrypted so far

-}
addBinOnes :: Bintree Char -> Char -> String-> String
addBinOnes (Node x left Empty) _ value = value
addBinOnes (Node x left right) char value
    | x == char = value
    | otherwise = (addBinOnes right char value) ++ "1"

{-
Function 'lastchar' finds the last character in a binary tree.
Takes 1 argument:
    'tree': the binary tree that is being checked
returns the last character in the binary tree.
-}
lastchar :: Bintree Char -> Char
lastchar Empty = ' '
lastchar (Node x left Empty) = x
lastchar (Node x left right) = lastchar right

{-
Function 'huffEncryptChar' encrypts a character.
Takes 3 arguments:
    'tree': the binary tree that holds the characters in the file to be encrypted
    'char': the character that is being encrypted
    'value': the bitstring that has been encrypted so far
returns the 'value' with the encrypted character added to it.
the function itself calls the 'addBinOnes' function to encrypt the character.
if the character is the last character in not the last character in the tree, the function adds a 0 to the bitstring.
else it adds nothing.
-}
huffEncryptChar ::  Bintree Char -> Char -> String-> String
huffEncryptChar Empty _ value = value
huffEncryptChar (Node x left right) char value
    | char == last = addBinOnes (Node x left right) char value
    | otherwise = addBinOnes (Node x left right) char value ++ "0"
    where last = lastchar (Node x left right)

{-
Function 'huffEncryptRec' encrypts the content of the file to be encrypted.
Takes 3 arguments:
    'tree': the binary tree that holds the characters in the file to be encrypted
    'content': the content of the file to be encrypted
    'value': the bitstring that has been encrypted so far
returns the encrypted value.
Does this by going over every character in the value and encrypting it with the 'huffEncryptChar' function.
-}
huffEncryptRec :: Bintree Char -> String -> String -> String
huffEncryptRec Empty _ value = value
huffEncryptRec tree [] value = value
huffEncryptRec tree (x:xs) value 
    = huffEncryptRec tree xs (huffEncryptChar tree x value)



{-
Function 'huffEncrypt' encrypts the content of the file to be encrypted.
Takes 2 arguments:
    'tree': the binary tree that holds the characters in the file to be encrypted
    'content': the content of the file to be encrypted
Is a wrapper function for the recursive 'huffEncryptRec' that takes an Integer as a third argument.
returns the encrypted value.
-}
huffEncrypt :: Bintree Char -> String -> String
huffEncrypt tree [] = []
huffEncrypt tree content =  huffEncryptRec tree content ""


{-
Function 'printStats' prints the statistics of the compression.
Takes 2 arguments:
    'input': the content of input file
    'output': the content of output file
-}
printStats :: String -> ByteString -> IO ()
printStats input output = do
    putStrLn  "file compressed, statistics:"
    putStrLn $ "Input file size: " ++ show (Prelude.length input) ++ " bytes"
    putStrLn $ "Output file size: " ++ show (BS.length output) ++ " bytes"
    putStrLn $ "Compression ratio: " ++ show (BS.length output*100 `div` Prelude.length input) ++ "%"




strToBits :: String -> Int ->Word8-> (String,Word8)
strToBits [] value byte = ([],byte)
strToBits input 0 byte = (input,byte)
strToBits (x:xs) value byte
    | x == '1' = strToBits xs (value-1) (byte + 2^(value-1))
    | otherwise = strToBits xs (value-1) byte
    

strToByteS :: String -> ByteString -> ByteString
strToByteS [] value = value
strToByteS input output = strToByteS rest (singleton byte `append` output)
    where (rest, byte) = strToBits input 8 0
          

bitsToStr :: Word8 -> Int->  String ->String
bitsToStr byte 0 value = value 
bitsToStr byte timer value
    | byte `mod` 2 == 1 = bitsToStr (byte `div` 2) (timer-1) ('1':value)
    | otherwise = bitsToStr (byte `div` 2) (timer-1) ('0':value)


bytesToStr :: ByteString -> String-> String
bytesToStr input value 
    | BS.null input = value
    | otherwise = bytesToStr (BS.tail input )(bits ++ value )
        where bits = bitsToStr (BS.head input) 8 ""





main :: IO()
main = do
    args <- getArgs
    let expectedArgAmount = 3
    let expectedArgExtensions = [".txt",".txt", ".txt"]
    let useError = "Usage: rlcompress <inputfile.txt> <outputfile.txt> <outputtreefile.txt>"
    [inputFile, outputFile, treeFile] <- validateInput args expectedArgAmount expectedArgExtensions useError

    input <- Prelude.readFile inputFile
    let tree = createBinaryTree input
    let output = show $ huffEncrypt tree input
    putStrLn output
    let result = strToByteS output empty
    BS.writeFile outputFile result
    Prelude.writeFile treeFile $ show tree
    printStats input result
    







