import Control.Monad
import Data.Monoid
import Control.Monad.Writer

liftM' :: (Monad m) => (a->b) -> m a -> m b
{- liftM f m = m >>= (\x -> return (f x)) -}
liftM' f m = do  
    x <- m  
    return (f x) 

ap' :: (Monad m) => m (a -> b) -> m a -> m b  
ap' mf m = mf >>= (\f -> (m >>= (\x -> return (f x))))

{- runWriter $ join (writer (writer (1,"aaa"),"bbb")) -} 
join' :: (Monad m) => m (m a) -> m a
join' mm = mm >>= (\m -> m)

{- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  -} 

keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False 

{- 
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a 
foldM binSmalls 0 [2,8,3,1]
-}
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x) 