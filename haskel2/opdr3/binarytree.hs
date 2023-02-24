-----------------------------------------------------------------------------
-- |
-- Module      :  binarytree
-- Copyright   :  yes
-- License     :  yes
--
-- Maintainer  :  1039779@hr.nl
-- Stability   :  Very very Unstable
-- Portability :  porttable
--
-- This module contains a binarytree with functions to add, remove and search for nodes
--
-----------------------------------------------------------------------------

module BinaryTree where


data Bintree a = Empty | Node a (Bintree a) (Bintree a) 
    deriving(Show, Read)
    

{-
Function 'push' adds a node to the binarytree.
Takes 2 arguments:
    'tree': the binarytree that is being added to
    'x': the value of the node that is being added
returns the binarytree with the added node.
-}
push :: (Ord a) => Bintree a -> a -> Bintree a
push Empty x = Node x Empty Empty
push (Node a left right) x
    | x < a = Node a (push left x) right
    | otherwise = Node a left (push right x)


{-
Function 'pushList' adds a list of nodes to the binarytree.
Takes 2 arguments:
    'tree': the binarytree that is being added to
    'list': the list of nodes that are being added
uses the function 'push' to add the nodes to the binarytree.
returns the binarytree with the added nodes.
-}
pushList :: (Ord a) => Bintree a -> [a] -> Bintree a
pushList = foldl push


{-
Function 'mapTree' maps a function over the binarytree.
Takes 2 arguments:
    'f': the function that is being mapped over the binarytree
    'tree': the binarytree that is being mapped over
returns the binarytree with the mapped function.
-}
mapTree :: (a -> b) -> Bintree a -> Bintree b
mapTree f Empty = Empty
mapTree f (Node a left right) = 
    Node (f a) (mapTree f left) (mapTree f right)


{-
Function 'filterTree' filters the binarytree.
Takes 2 arguments:
    'f': the function that is being used to filter the binarytree
    'tree': the binarytree that is being filtered
returns a list with all the values that passed the filter.
-}
filterTree :: (a -> Bool) -> Bintree a -> [a]
filterTree f Empty = []
filterTree f (Node a left right) 
    | f a = [a] ++ filterTree f left ++ filterTree f right
    | otherwise = filterTree f left ++ filterTree f right


{-
Function 'preOrder' returns a list with the values of the binarytree in preOrder.
Takes 1 argument:
    'tree': the binarytree that is being traversed
-}
preOrder :: Bintree a -> [a]
preOrder Empty = []
preOrder (Node a left right) = 
    [a] ++ preOrder left ++ preOrder right

{-
Function 'postOrder' returns a list with the values of the binarytree in postOrder.
Takes 1 argument:
    'tree': the binarytree that is being traversed
-}
postOrder :: Bintree a -> [a]
postOrder Empty = []
postOrder (Node a left right) = 
    postOrder left  ++ postOrder right ++ [a]


{-
Function 'inOrder' returns a list with the values of the binarytree in inOrder.
Takes 1 argument:
    'tree': the binarytree that is being traversed
-}
inOrder :: Bintree a -> [a]
inOrder Empty = []
inOrder (Node a left right) = 
    inOrder left ++ [a] ++ inOrder right


