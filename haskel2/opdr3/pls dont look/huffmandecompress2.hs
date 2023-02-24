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
-- This module contains the algorithm to decompress a file using huffman compression
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
import Control.Arrow (ArrowChoice(right))



huffDecrptChar :: String -> Bintree Char -> (Char, String)
huffDecrptChar "" (Node x left right) = (x, "")
huffDecrptChar input (Node x left Empty) = (x, input)
huffDecrptChar (head:rest) (Node x left right) 
    | head == '0' = (x , rest)
    | otherwise = huffDecrptChar rest right


{-
Function 'huffDecrypt' decrypts a file using huffman compression.
Takes 2 arguments:
    'input': the input that is being decrypted
    'tree': the tree that is being used to decrypt the input
returns the decrypted input.
The function removes the first 
-}
huffDecrypt :: String -> Bintree Char -> [Char]
huffDecrypt "" _ = []
huffDecrypt input tree 
    = [char] ++ huffDecrypt newinput tree 
        where (char, newinput) = huffDecrptChar input tree



intToString :: Integer -> String -> String
intToString 0 output = output
intToString input output 
    | even input = intToString (input `div` 2) (output ++ "0")
    | otherwise = intToString (input `div` 2) (output ++ "1")

finaltouch :: Integer -> String
finaltouch input = tail $ tail $ reverse body
    where (head:body) = intToString input ""

main :: IO ()
main = do
    args <- getArgs
    let expectedArgAmount = 3
    let expectedArgExtensions = [".txt",".txt", ".txt"]
    let useError = "Usage: rlcompress <inputfile.txt> <outputfile.txt> <Inputtreefile.txt>"
    [inputFile, outputFile, treeFile] <- validateInput args expectedArgAmount expectedArgExtensions useError

    inputstring <- readFile inputFile
    treestring <- readFile treeFile
    let tree  = read treestring :: Bintree Char
    let input = read inputstring :: Integer
    
    let outputstring = huffDecrypt (finaltouch input) tree

    writeFile outputFile outputstring


