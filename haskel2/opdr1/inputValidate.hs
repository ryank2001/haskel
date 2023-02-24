-----------------------------------------------------------------------------
-- |
-- Module      :  InputValidate
-- Copyright   :  yes
-- License     :  yes
--
-- Maintainer  :  1039779@hr.nl
-- Stability   :  Very very Unstable
-- Portability :  porttable
--
-- this module contains functions that check the input of the user given from the terminal are correct
--
-----------------------------------------------------------------------------
module InputValidate where
import Data.Char
import System.Environment
import Data.List
import qualified System.Directory as Dir
import System.FilePath 


{- |
Function 'checkFileExists' checks if the input file exists.
takes 2 arguments: 
    'file': the filepaths given by the user
    'usage': template for how the function call should look like added with the error message
takes a list filepaths and checks only the first file (the input file).
If the file exists the function returns the filepaths.
If the file does not exist the function returns an error message.
-}
checkFileExists :: [FilePath] -> String ->  IO [FilePath]
checkFileExists file usage= do
    existance <- Dir.doesFileExist $ head file 
    if existance then return file
    else error $ "File " ++ head file ++ " does not exist.\n" ++ usage

{- |
Function 'checkFileExtension' checks or all the files have the correct extension.
takes 3 arguments: 
    'args': the arguments given by the user
    'expExtensions': the extensions expected
    'usage': template for how the function call should look like added with the error message
If the extensions are correct the function returns the filepaths.
If the extensions are not correct the function returns an error message.
-}
checkFileExtension :: [FilePath] -> [String] -> String -> [FilePath]
checkFileExtension args expExtensions usage= 
    let extension = [takeExtension file | file <- args]
    in if extension == expExtensions then args
    else error $ "Expected " ++ show expExtensions ++ " extensions but got " ++ show extension ++ " extensions.\n" ++ usage

{-
Function 'checkArgAmount' checks or the function call has the correct amount of arguments.
expected 'input': [input file, output file]

If the correct amount of arguments is given the function returns the arguments.
If the wrong amount of arguments is given the function returns an error message.
-}
checkArgAmount :: [FilePath] -> Int -> String -> [FilePath]
checkArgAmount args expectedArgAmount usage
    | length args /= expectedArgAmount = error $ "Expected " ++ show expectedArgAmount ++ " arguments but got " ++ show (length args) ++ " arguments.\n" ++ usage
    | otherwise = args


{- |
Function 'validateInput' checks the input of the user given in the terminal.
takes 4 arguments: 
    'args': the arguments given by the user
    'argAmount': the amount of arguments expected
    'argExtensions': the extensions expected
    'usage': template for how the function call should look like added with the error message
The function checks if the amount of arguments is correct, if the extensions are correct and if the input file exists.
If the input is correct the function returns the filepaths.
If the input is not correct the function will throw a error.
-}
validateInput :: [FilePath] -> Int -> [String]->String -> IO [FilePath]
validateInput args argAmount argExtensions usage= do
    let dunno = checkArgAmount args argAmount usage 
    let checkedarg = checkFileExtension dunno argExtensions usage
    checkFileExists checkedarg usage 
    
