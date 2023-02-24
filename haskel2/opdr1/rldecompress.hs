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
-- This module decompresses a file using the run length encoding algorithm
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
Function 'takeDigits' takes a string and returns a tuple with the string without the digits and the digits.
Takes 2 arguments:
    'string': the string that is being checked
    'output': the number that is being built up (empty at the start)
The function checks if the first character of the string is a digit.
If it is a digit the function adds the digit to the number and passes the rest of the string to the function again.
-}
takeDigits :: (String,String) -> (String, String)
takeDigits ([],_) = ([], [])
takeDigits (x:xs, output)
    | isDigit x = takeDigits (xs, output ++ [x])
    | otherwise = (x:xs, output)


{-
Function 'writeChars' writes the characters to the decompressed string.
Takes 2 arguments:
    'char': the character that is being written
    'number': the number of times the character is being written
    'string': the string that is being built up
returns the decompressed string.
-}
writeChars :: Char -> String -> String -> String
writeChars char number string = string ++ replicate (read number) char

{-
Function 'rldecompress' decompresses a string using the run length encoding algorithm.
Takes 2 arguments:
    'string': the string that is being decompressed
    'output': the decompressed string that is being built up
The function first removes the numbers in front of the character with the function 'takeDigits'.
The function 'takeDigits' returns a tuple with the string without the digits and the digits.
The function then passes the character, the number and the decompressed string to the function 'writeChars'.
Then the function repeats this process until the string is empty.
-}
rldecompress :: String -> String -> String
rldecompress [] output = output
rldecompress input output =
    rldecompress rest (writeChars char number output)
    where
        (char:rest, number) = takeDigits (input, [])
        
        




{-
Function 'printStats' prints the statistics of the decompression.
Takes 2 arguments:
    'input': the content of input file
    'output': the content of output file
-}
printStats :: String -> String -> IO ()
printStats input output = do
    putStrLn  "file decompressed, statistics:"
    putStrLn $ "Input file size: " ++ show (length input) ++ " bytes"
    putStrLn $ "Output file size: " ++ show (length output) ++ " bytes"
    putStrLn $ "Compression ratio: " ++ show (length output*100 `div` length input) ++ "%"




{-
Main function that reads a files, deocmpresses the content, and writes the decompressed content to a file.
terminal command: rlcompress <inputfile.txt> <outputfile.rlc>
-}
main :: IO ()
main = do
    args <- getArgs
    let expectedArgAmount = 2 
    let expectedArgExtensions = [".rlc",".txt"]
    let useError = "Usage: rldecompress <inputfile.rlc> <outputfile.txt>"
    [inputFile, outputFile] <- validateInput args expectedArgAmount expectedArgExtensions useError

    fileContent <- readFile inputFile
    let sortedFile = rldecompress fileContent ""
    writeFile outputFile sortedFile
    printStats fileContent sortedFile
    