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
-- This module contains opdr2 form assignment 2
--
-----------------------------------------------------------------------------

module Main where
import BinaryTree
import Data.Char

main :: IO()
main = do
    studentinfo <- readFile "studentinfo.txt"
    let treeToWrite = pushList Empty studentinfo
    writeFile "tree.txt" (show (mapTree ord treeToWrite))

    tree <- readFile "tree.txt"
    let intTree = read tree :: Bintree Int
    let charTree = mapTree chr intTree
    putStrLn (inOrder charTree)
    putStrLn (filterTree isDigit charTree)
    
    


    




    
