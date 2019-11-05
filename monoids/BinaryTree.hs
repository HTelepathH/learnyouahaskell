{- 
module BinaryTree
( Tree(..)
, singleton
, treeInsert
, treeElem
) where 
-}
import qualified Data.Foldable as F
import Data.Monoid

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

{- insert -}
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node a left right
    | x > a = Node a left (treeInsert x right) 
    | otherwise = Node a (treeInsert x left) right

{- search -}
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x > a = treeElem x right
    | otherwise = treeElem x left

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = foldMap f l `mappend` 
                             f x         `mappend`
                             foldMap f r 


nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

testTree = Node 5  
            (Node 3  
                (Node 1 EmptyTree EmptyTree)  
                (Node 6 EmptyTree EmptyTree)  
            )  
            (Node 9  
                (Node 8 EmptyTree EmptyTree)  
                (Node 10 EmptyTree EmptyTree)  
            )  


