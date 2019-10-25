{- 
class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg


instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
 -}

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right) 
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing
  
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing 

banana :: Pole -> Maybe Pole  
banana _ = Nothing  
{- 
return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
 -}
{- do expressions are just different syntax for chaining monadic values. -}
{- Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))) -} 

foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)  

routine :: Maybe Pole
routine = do
    start <- return (0, 0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second

wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x  

