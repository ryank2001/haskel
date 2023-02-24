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





{-
Function 'checkCharacter' checks how many times a character occurs in a String.
Takes 2 arguments:
    'list': the string that is being checked
    'char': the character that is being checked
returns the amount of times the character occurs in the string.
-}
checkCharacter :: [Char] -> Char-> Int
checkCharacter list char =length [True | x <- list , x == char]

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
addBinOnes :: Bintree Char -> Char -> Integer-> Integer
addBinOnes Empty _ value = value
addBinOnes (Node x left right) char value
    | x == char = value
    | otherwise = (shift (huffEncryptChar right char value) 1) + 1

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
huffEncryptChar ::  Bintree Char -> Char -> Integer-> Integer
huffEncryptChar Empty _ value = value
huffEncryptChar (Node x left right) char value
    | x == last = addBinOnes (Node x left right) char value
    | otherwise = shift (addBinOnes (Node x left right) char value) 1
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
huffEncryptRec :: Bintree Char -> String -> Integer -> Integer
huffEncryptRec Empty _ value = value
huffEncryptRec tree [] value = value
huffEncryptRec tree (char:rest) value 
    = huffEncryptRec tree rest (huffEncryptChar tree char value)



{-
Function 'huffEncrypt' encrypts the content of the file to be encrypted.
Takes 2 arguments:
    'tree': the binary tree that holds the characters in the file to be encrypted
    'content': the content of the file to be encrypted
Is a wrapper function for the recursive 'huffEncryptRec' that takes an Integer as a third argument.
returns the encrypted value.
-}
huffEncrypt :: Bintree Char -> String -> Integer
huffEncrypt tree [] = 0
huffEncrypt tree content =  huffEncryptRec tree content 1


{-
Function 'printStats' prints the statistics of the compression.
Takes 2 arguments:
    'input': the content of input file
    'output': the content of output file
-}
printStats :: String -> String -> IO ()
printStats input output = do
    putStrLn  "file compressed, statistics:"
    putStrLn $ "Input file size: " ++ show (length input) ++ " bytes"
    putStrLn $ "Output file size: " ++ show (length output) ++ " bytes"
    putStrLn $ "Compression ratio: " ++ show (length output*100 `div` length input) ++ "%"





main :: IO()
main = do
    args <- getArgs
    let expectedArgAmount = 3
    let expectedArgExtensions = [".txt",".txt", ".txt"]
    let useError = "Usage: rlcompress <inputfile.txt> <outputfile.txt> <outputtreefile.txt>"
    [inputFile, outputFile, treeFile] <- validateInput args expectedArgAmount expectedArgExtensions useError

    input <- readFile inputFile
    let tree = createBinaryTree input
    let output = show $ huffEncrypt tree input
    writeFile outputFile output
    writeFile treeFile $ show tree


    printStats input output
    







