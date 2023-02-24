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
-- This module compresses a file using the run length encoding algorithm
--
-----------------------------------------------------------------------------

module Main where
import System.Environment
import System.IO
import Data.Char
import Data.List
import System.Directory
import InputValidate


{- 
Function 'compareChar' Takes a character and a string and checkes how many time the characker occurs in a row 
at the start of the string.
Takes 2 arguments:
    'char': the character that is being compared
    'string': the string that is being compared
returns a tuple with the amount of times the character occursin a row(int)and the string without the character that was being checked.
-}
compareChar :: Char -> String -> (Int, String)
compareChar char [] = (1, [])
compareChar char (x:xs)
    | char == x = (1 + count, rest)
    | otherwise = (1, x:xs)
    where
        (count, rest) = compareChar char xs


{-
Function 'rlcompress' compresses a string using the run length encoding algorithm.
Takes 2 arguments:
    'string': the string that is being compressed
    'comp': the compressed string that is being built
The function checks the removes the first character of the string and passes these to the function 'compareChar'.
The function 'compareChar' returns a tuple with the amount of times the character occurs in a row and the string without the character that was being checked.
After this the function writes the char and the amount of times it occurs to the compressed string and passes the rest of the string to the function again.
The function stops when the string is empty.
-}
rlcompress :: String -> String -> String
rlcompress [] comp= comp
rlcompress (x:xs) comp 
    | count == 0 = comp ++ show 1 ++ [x]
    |otherwise = rlcompress rest (comp ++ show count ++ [x])
    where
        (count, rest) = compareChar x xs


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




{-
Main function that reads a files, copresses the content, and writes the copressed content to a file.
terminal command: rlcompress <inputfile.txt> <outputfile.rlc>
-}
main :: IO ()
main = do
    args <- getArgs
    let expectedArgAmount = 2 
    let expectedArgExtensions = [".txt",".rlc"]
    let useError = "Usage: rlcompress <inputfile.txt> <outputfile.rlc>"
    [inputFile, outputFile] <- validateInput args expectedArgAmount expectedArgExtensions useError
    fileContent <- readFile inputFile
    let sortedFile = rlcompress fileContent ""
    writeFile outputFile sortedFile
    printStats fileContent sortedFile
    