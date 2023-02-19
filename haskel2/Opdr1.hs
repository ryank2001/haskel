{- |
Module      :  opdr1
Description :  Contains all the exercises for the first assignment of the course haskel2
Copyright   :  yes
License     :  yes

Maintainer  :  1039779@hr.nl
Stability   :  Very very unstable
Portability :  portable

contains exercises about reading and writing files and the run lenght encoding
-}
module Main where
import Data.Char
import System.Environment
import Data.List
import qualified System.Directory as Dir




{- |
Function that checks or the file exists
Takes a file name as argument
Either returns the file name if the file exists
Otherwise throws a error
-}
checkFileExists :: String -> IO Bool
checkFileExists file = do
    work <- Dir.doesFileExist file 
    if work then return True
    else error "File does not exist"
    
    
    


{- |
Function that validates the 3 arguments:
    1. a List 
The function will check or there are 2 arguments and the read file exists 

Returns the readfile and writefile's name if the arguments are valid
Otherwise throws a error
-}
inputValidate :: [String] -> [String]
inputValidate [] = error $ "No arguments Were given" ++ "/n" ++ "usage: ./runFile readfile writefile"
inputValidate [x] = error $ "No writefile given" ++ "/n" ++ "usage: ./runFile readfile writefile"
inputValidate [x,y] = [checkFileExists x,y]
inputValidate _ = error $ "Too many arguments given" ++ "/n" ++ "usage: ./runFile readfile writefile"


{- |
The main function does the following:
 1. Reads the command line arguments: Readfile and Writefile name
 2. reads the file
 3. Orders the letters of the file
 4. Writes the ordered file to the writefile
-}
main :: IO ()
main = do    
    args <- getArgs
    let expectedArgAmount = 2 
    let expectedArgExtensions = [".txt",".txt"]
    let [readfileName,writefileName] <- inputValidate args 
    file <- readFile args
    writeFile writefileName (sort file)
    putStrLn "Reversed the file contents and write the output to the writefile"