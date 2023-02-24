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


huffDecrptChar :: Integer -> Bintree Char -> (Char, Integer)
huffDecrptChar 1 (Node x left right) = (x, 1)
huffDecrptChar input (Node x left Empty) = (x, input)
huffDecrptChar input (Node x left right) 
    | even input = (x , input)
    | otherwise = huffDecrptChar (input `div` 2) right


{-
Function 'huffDecrypt' decrypts a file using huffman compression.
Takes 2 arguments:
    'input': the input that is being decrypted
    'tree': the tree that is being used to decrypt the input
returns the decrypted input.
The function removes the first 
-}
huffDecrypt :: Integer -> Bintree Char -> [Char]
huffDecrypt 0 _ = []
huffDecrypt 1 _ = []
huffDecrypt input tree 
    = huffDecrypt newinput tree ++ [char]
        where (char, newinput) = huffDecrptChar (input `div` 2) tree



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
    let outputstring = huffDecrypt input tree
    writeFile outputFile outputstring


