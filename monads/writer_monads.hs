import Data.Monoid
import Control.Monad.Writer

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newlog) = f x in (y, log `mappend` newlog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans"  = ("milk", Sum 25)
addDrink "jerky" = ("wiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

{- 

newtype Writer w a = Writer { runWriter :: (a, w) } 

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (a, w)) >>= f = let (Writer (b, w')) = f x 
                            in  Writer (b, w `mappend` w')

 -}

logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  

{- 
(*) <$> logNumber 3 <*> logNumber 5 
-}
multWithLog :: Writer [String] Int  
multWithLog = do 
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["tell mutiply"]
    return (a * b) 

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finish with " ++ show a]
        return a 
    | otherwise = do
        tell [show a ++  " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result  