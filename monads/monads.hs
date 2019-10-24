import Control.Monad

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

{- the function monad is also called the Reader monad -}
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w 
    :: (r->a) -> (a->(r->b)) -> (r->b)

instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing 

{- [(n, ch) | n <- [1, 2], ch <- ['a', 'b'] ] -}
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)

{- MonadPlus : Monads that satifies Monoid laws -}
class Monad m => MonadPlus m where
    mzero :: m a 
    mplus :: m a -> m a -> m a 

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

{- 

[1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)  

[ x | x <- [1..50], '7' `elem` show x ]

-}
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x  


instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []

instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg) 

{- 
laws of Monad

1. LEFT IDENTITY :
return x >>= f    equals    f x

2. RIGHT IDENTITY :
m >>= return      equals    m

3. ASSOCIATIVITY :
(m >>= f) >>= g   equals    m >>= (\x -> f x >>= g)
f <=< (g <=< h)   equals    (f <=< g) <=< h
-}
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f) 
