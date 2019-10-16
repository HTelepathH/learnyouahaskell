import Data.Char  
import Data.List  
import Control.Applicative

{- fmap is function composition when used on functions -}

{-  
instance Functor ((->) r) where  
    fmap = (.)  

instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))

fmap :: (a -> b) -> (r -> a) -> (r -> b)  
fmap show (*100) $ 5
-}

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  
{-
Law of Functors 
1. fmap id = id
2. fmap (f . g) = fmap f . fmap g
 -}
data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

{-  -}

class Functor f where
    fmap :: (a -> b) -> f a -> f b  
    
class (Functor f) => Applicative f where
    pure :: a -> f a 
    (<*>) :: f (a -> b) -> f a -> f b

pure f <*> x = fmap f x
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> sth = fmap f sth

instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  

instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x) 
        
instance Applicative ((->) r) where
    pure x = (\_ -> x)
        :: a -> (r->a)
    f <*> g = (\x -> f x (g x))
        :: (r->(a->b)) -> (r->a) -> (r->b)

instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)



myAction :: IO String
myAction = (++) <$> getLine <*> getLine

{- 
Law of Applicative Functor
pure f <*> x = fmap f x
1. pure id <*> v = v
2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
3. pure f <*> pure x = pure (f x)
4. u <*> pure y = pure ($ y) <*> u
 -}

newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair b) where
    fmap f (Pair (x, y)) = Pair (f x, y)

{- data CoolBool = CoolBool { getCoolBool :: Bool }  
 -}
newtype CoolBool = CoolBool { getCoolBool :: Bool }  

helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello" 

{- type vs. newtype vs. data -}