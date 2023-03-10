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
import Data.List as LT
import System.Directory.Internal.Prelude (getArgs)
import Data.ByteString (ByteString, empty)
import GHC.Exts.Heap (GenClosure(value))
import Data.Bits (Bits(shift))
import qualified Data.Map as Map
import Data.Word
import Data.ByteString as BS
import Data.Maybe (isNothing)






{-
Function 'checkCharacter' checks how many times a character occurs in a String.
Takes 2 arguments:
    'list': the string that is being checked
    'char': the character that is being checked
returns the amount of times the character occurs in the string.
-}
checkCharacter :: [Char] -> Char-> Int
checkCharacter list char =Prelude.length [True | x <- list , x == char]

{-
Function 'doubles' checks how many times a character occurs in a String.
Takes 1 argument:
    'list': the string that is being checked
returns a list of tuples with the character and the amount of times it occurs.
-}
doubles :: [Char] -> [(Char,Int)]
doubles [] = []
doubles list = [(x,checkCharacter list x) | x <- nub list]

{-
extracts the weight of a character from a tree node
takes 1 argument:
    'tree': the tree node that is being checked
returns the weight of the tree node

-}
weightNode :: Bintree (Char,Int) -> Int
weightNode (Node (c,w) t1 t2) = w
weightNode Empty = 0
{-
extracts the character from a tree node
takes 1 argument:
    'tree': the tree node that is being checked
returns the character in the tree node
-}
charNode :: Bintree (Char,Int) -> Char
charNode (Node (c,w) t1 t2) = c
charNode Empty = ' '

{-
function 'combineTree' combines 2 bintrees by attatching them to a node and putting char '0' and the weight of the 2 trees combined as the character and weight of the node.
takes 2 arguments:
    'tree1': the first tree that is being combined
    'tree2': the second tree that is being combined
returns a tree with the 2 trees combined
-}
combineTree :: Bintree (Char,Int) -> Bintree (Char,Int) -> Bintree (Char,Int)
combineTree tree1 tree2 = Node ('0', weightNode tree1 + weightNode tree2) tree1 tree2

{-
function 'compareWeightNodes' compares the weight of 2 bintrees.
takes 2 arguments:
    'tree1': the first tree that is being compared
    'tree2': the second tree that is being compared
returns a boolean that is true if the weight of tree1 is higher than the weight of tree2
if any of the trees are empty it will return false
-}
compareWeightNodes :: Bintree (Char,Int) -> Bintree (Char,Int) -> Bool
compareWeightNodes Empty Empty = False
compareWeightNodes Empty _ = False
compareWeightNodes _ Empty = False
compareWeightNodes tree1 tree2 = weightNode tree1 >= weightNode tree2

{-
Function 'leafToTree' converts a list of tuples to a binary tree that holds the characters in the file to be encrypted.
takes 2 arguments:
    'queue1': a list of leafs that are being converted to a tree ordenerd by weight ( lower weight first)
    'queue2': a list of trees that are being converted to a tree
returns a binary tree with the characters in the file to be encrypted.

The function works constantly combining the 2 leafs/trees with the lowest weight.
The function starts with all the leafs in 'queue1' then it combines the first 2 leafs and puts it at the bottom of 'queue2'.
after this it will check both the first and second leaf of both 'queue1' and 'queue2' and combine the 2 leafs with the lowest weight
 and putting this new tree at the bottom of 'queue2'
-}
leafToTree :: [Bintree (Char,Int)] -> [Bintree (Char,Int)]-> Bintree (Char,Int)
leafToTree [] [] = Empty
leafToTree [] [tree1] = tree1
leafToTree [tree1] [] = tree1
leafToTree [] (tree1:tree2:rest) = leafToTree [] (rest ++ [combineTree tree1 tree2])
leafToTree (tree1:tree2:rest) []= leafToTree rest [combineTree tree1 tree2]
leafToTree (tree1:rest) (tree3:rest2)
    | compareWeightNodes tree3 tree2 = leafToTree (LT.tail rest) (tree3:rest2 ++ [combineTree tree1 tree2])
    | compareWeightNodes tree1 tree4 = leafToTree (tree1:rest) (LT.tail rest2 ++ [combineTree tree3 tree4])
    | otherwise = leafToTree rest (rest2 ++ [combineTree tree1 tree3])
        where tree2 = if LT.null rest then Empty else Prelude.head rest
              tree4 = if LT.null rest2 then Empty else Prelude.head rest2
            




{-
Function 'mapsToLeaf' converts a list of tuples to a binary tree that holds the characters in the file to be encrypted.
Takes 1 argument:
    'input': a list of tuples with the character and the amount of times it occurs
returns a binary tree with the characters in the file to be encrypted.
-}
mapsToLeaf :: [(Char,Int)] -> Bintree (Char,Int)
mapsToLeaf [] = Empty
mapsToLeaf input = leafToTree [Node (x,y) Empty Empty | (x,y) <- input] []




{-
Function 'createBinaryTree' creates a binary tree that holds the characters in the file to be encrypted.
Takes 1 argument:
    'input': the content of the file to be encrypted
the return value is a binary tree with the most used characters at the top and the least used characters at the bottom.
-}
createBinaryTree :: String -> Bintree (Char,Int)
createBinaryTree [] = Empty
createBinaryTree input = mapsToLeaf  (sortBy ((\(_,a) (_,b) -> compare a b)) $ doubles input) 

temp :: String -> [(Char,Int)]
temp [] =   []
temp input = (sortBy ((\(_,a) (_,b) -> compare a b)) $ doubles input) 



{-
Function 'bintreeToMap' converts a binary tree to a map.
Takes 2 arguments:
    'tree': the binary tree that holds the characters in the file to be encrypted
    'path': the path to the current node
returns a map with the characters in the file to be encrypted and the path to the character.
-}
bintreeToMap :: Bintree (Char,Int) -> String-> Map.Map Char String
bintreeToMap Empty _= Map.empty
bintreeToMap (Node (c,w) Empty Empty) "" = Map.singleton c "0"
bintreeToMap (Node (c,w) Empty Empty) path = Map.singleton c path
bintreeToMap (Node (c,w) t1 t2) path = Map.union (bintreeToMap t1 (path ++ "0")) (bintreeToMap t2 (path ++ "1"))


{-
Function 'huffEncryptRec' encrypts the content of the file to be encrypted.
Takes 2 arguments:
    'map': a map with the characters in the file to be encrypted and the path to the character in binary
    'content': the content of the file to be encrypted
returns the encrypted value.
-}
huffEncryptRec  :: Map.Map Char String -> String -> String
huffEncryptRec _ [] = ""
huffEncryptRec map (x:xs) = (map Map.! x) ++ huffEncryptRec map xs


{-
Function 'huffEncrypt' encrypts the content of the file to be encrypted.
Takes 2 arguments:
    'tree': the binary tree that holds the characters in the file to be encrypted
    'content': the content of the file to be encrypted
Is a wrapper function for the recursive 'huffEncryptRec' that takes a map instead of a Bintree for faster lookup.
returns the encrypted value.
-}
huffEncrypt :: Bintree (Char,Int) -> String -> String
huffEncrypt tree [] = ""
huffEncrypt tree content = huffEncryptRec (bintreeToMap tree "") content



{-
Function 'printStats' prints the statistics of the compression.
Takes 2 arguments:
    'input': the content of input file
    'output': the content of output file
-}
printStats :: String -> ByteString -> IO ()
printStats input output = do
    putStrLn  "file compressed, statistics:"
    putStrLn $ "Input file size: " ++ show (Prelude.length input) ++ " bytes"
    putStrLn $ "Output file size: " ++ show (BS.length output) ++ " bytes"
    putStrLn $ "Compression ratio: " ++ show (BS.length output*100 `div` Prelude.length input) ++ "%"


{-
Function 'strToBits' converts a string of 1's and 0's to actual binary.
Takes 3 arguments:
    'input': the string of 1's and 0's
    'counter': the amount of bits left to convert that fit in a byte
    'byte': the current byte
returns a tuple with the rest of the string and the byte.
-}
strToBits :: String -> Int ->Word8-> (String,Word8)
strToBits [] counter byte = ([],byte)
strToBits input 0 byte = (input,byte)
strToBits (x:xs) counter byte
    | x == '1' = strToBits xs (counter-1) (byte + 2^(counter-1))
    | otherwise = strToBits xs (counter-1) byte
    

{-
Function 'strToByteS' converts a string of 1's and 0's to a ByteString.
Takes 2 arguments:
    'input': the string of 1's and 0's
    'value': the ByteString to append to
returns the ByteString with the converted string.
Might have some trailing zeros that i do not know how to remove.
-}
strToByteS :: String -> ByteString -> ByteString
strToByteS [] value = value
strToByteS input output = strToByteS rest (BS.singleton byte `append` output)
    where (rest, byte) = strToBits input 8 0
          


main :: IO()
main = do
    args <- getArgs
    let expectedArgAmount = 3
    let expectedArgExtensions = [".txt",".txt", ".txt"]
    let useError = "Usage: rlcompress <inputfile.txt> <outputfile.txt> <outputtreefile.txt>"
    [inputFile, outputFile, treeFile] <- validateInput args expectedArgAmount expectedArgExtensions useError
    input <- Prelude.readFile inputFile
    putStrLn (show (temp input))
    let tree = createBinaryTree input
    Prelude.writeFile treeFile $ show tree
    let output = huffEncrypt tree input
    let result = strToByteS output empty

    BS.writeFile outputFile result
    
    printStats input result
    
    







