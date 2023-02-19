-----------------------------------------------------------------------------
-- |
-- Module      :  SortLetters
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


{-
Function that checks or the function call has the correct amount of arguments.
expected input: [input file, output file]

If the correct amount of arguments is given the function returns the arguments.
If the wrong amount of arguments is given the function returns an error message.
-}
checkArgAmount :: [FilePath] -> [FilePath]
checkArgAmount args
    | length args /= 2 = error $ "Expected 2 arguments but got " ++ show (length args) ++ " arguments.\n" ++ usage
    | otherwise = args
        where usage = "Usage: SortLetters <input file> <output file>"


{-
Main function that reads a files, sorts the letters, and writes the sorted letters to a file.
terminal command: SortLetters <input file> <output file>
-}
main :: IO ()
main = do
    args <- getArgs
    let [inputFile, outputFile] = checkArgAmount args
    fileContent <- readFile inputFile
    let sortedFile = sort fileContent
    writeFile outputFile sortedFile
    putStrLn $ "Sorted file is written to " ++ outputFile
    