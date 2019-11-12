data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show) 

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show) 
type Zipper a = (Tree a, [Crumb a])

modify :: (a->a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost p = topMost uped
    where Just uped = goUp p

goLeft :: Zipper a -> Maybe (Zipper a) 
goLeft (Node x l r , bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)  
goRight (Node x l r , bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a) 
goUp (l, LeftCrumb x r:bs) = Just (Node x l r, bs)
goUp (r, RightCrumb x l:bs) = Just (Node x l r, bs)
goUp (_, []) = Nothing  

{- 
return (freeTree,[]) >>= goRight >>= goRight >>= goRight 
-}

type ListZipper a = ([a],[a]) 

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a 
goBack (xs, b:bs) = (b:xs, bs) 

{- 
let xs = [1,2,3,4]
goForward (xs,[])
 -}
