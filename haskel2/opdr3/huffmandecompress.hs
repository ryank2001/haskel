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
import qualified Data.ByteString as BS
import Data.Binary (Word8)


decompressChar :: Bintree (Char,Int) -> String -> (String, Char)
decompressChar tree [] = ([], ' ')
decompressChar (Node (char,_) Empty Empty) string = (string, char)
decompressChar (Node _ left right) (x:xs)
    | x == '0' = decompressChar left xs
    | otherwise = decompressChar right xs

huffmanDecompress :: Bintree (Char,Int) -> String -> String
huffmanDecompress _ [] = []
huffmanDecompress tree string = 
    char : huffmanDecompress tree rest
    where (rest, char) = decompressChar tree string


{-
Function 'bitsToStr' converts a byte to a string that represents the bits(e.g. "00101110").
Takes 3 arguments:
    'byte': the byte that is being converted
    'timer': the amount of bits that are left to be converted
    'value': the string that is being added to
returns a string that represents the bits.
-}
bitsToStr :: Word8 -> Int->  String ->String
bitsToStr byte 0 value = value 
bitsToStr byte timer value
    | byte `mod` 2 == 1 = bitsToStr (byte `div` 2) (timer-1) ('1':value)
    | otherwise = bitsToStr (byte `div` 2) (timer-1) ('0':value)

{-
Function 'bytesToStr' convert a bytestring to a string that represents the bits(e.g. "00101110").
Takes 2 arguments:
    'input': the bytestring that is being converted
    'value': the string that is being added to
returns a string that represents the bits.
-}
bytesToStr :: ByteString -> String-> String
bytesToStr input value 
    | BS.null input = value
    | otherwise = bytesToStr (BS.tail input )(bits ++ value )
        where bits = bitsToStr (BS.head input) 8 ""

main :: IO ()
main = do
    args <- getArgs
    let expectedArgAmount = 3
    let expectedArgExtensions = [".txt",".txt", ".txt"]
    let useError = "Usage: rlcompress <inputfile.txt> <outputfile.txt> <Inputtreefile.txt>"
    [inputFile, outputFile, treeFile] <- validateInput args expectedArgAmount expectedArgExtensions useError

    inputstring <- BS.readFile inputFile
    treestring <- readFile treeFile
    let tree  = read treestring :: Bintree (Char,Int)
    let string = bytesToStr inputstring ""
    putStrLn string
    let output = huffmanDecompress tree string
    writeFile outputFile output
    putStrLn "decompressing done"
    


    
    
    


