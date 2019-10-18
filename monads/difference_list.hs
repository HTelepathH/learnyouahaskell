import Data.Monoid
import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList ([]++)

{- mappend -}
instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

{- mapM_ putStrLn . snd . runWriter $ finalCountDown 500000 -}
{- quick version -}
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do 
    tell $ toDiffList ["0"]
finalCountDown x = do 
    finalCountDown $ x - 1
    tell $ toDiffList [show x]

{- slow version -}
finalCountDowns :: Int -> Writer [String] ()  
finalCountDowns 0 = do  
    tell ["0"]  
finalCountDowns x = do  
    finalCountDowns (x-1)  
    tell [show x] 

{- 
class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w 
    :: (r->a) -> (a->(r->b)) -> (r->b)

 -}

{- the function monad is also called the Reader monad -}
addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)