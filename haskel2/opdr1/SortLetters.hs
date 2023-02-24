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
-- This module sorts the letters in a input file and loads it into a output file
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
Main function that reads a files, sorts the letters, and writes the sorted letters to a file.
terminal command: SortLetters <input file> <output file>
-}
main :: IO ()
main = do
    args <- getArgs
    let expectedArgAmount = 2 
    let expectedArgExtensions = [".txt",".txt"]
    let useError = "Usage: SortLetters <inputfile.txt> <outputfile.txt>"
    [inputFile, outputFile] <- validateInput args expectedArgAmount expectedArgExtensions useError
    fileContent <- readFile inputFile
    let sortedFile = sort fileContent
    writeFile outputFile sortedFile
    putStrLn $ "Sorted file is written to " ++ outputFile
    