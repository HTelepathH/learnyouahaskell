import Data.Ratio
import Data.List
import Control.Monad

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
    pure = return
    Prob fs <*> Prob xs = Prob $ [pairMake f x | f <- fs, x <- xs]
        where pairMake (f, r1) (x, r2) = (f x, r1 * r2)

instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4 )  
    ]

flatten :: Prob (Prob a) -> Prob a 
flatten (Prob x) = Prob $ concat $ map multAll x
    where multAll (Prob inner, p) = map (\(ch, r) -> (ch, r * p)) inner


data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipTwo' :: Prob [Coin]
flipTwo' = do
    a <- coin
    b <- loadedCoin  
    return [a,b]

flipTwo :: Prob [Coin]
flipTwo = coin >>= (\a -> loadedCoin >>= (\b -> return [a, b]))

flipThree :: Prob Bool  
flipThree = permute $ do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c]) 

permute :: Prob Bool -> Prob Bool
permute (Prob xs) = Prob [(False, 1%1 - p), (True, p)]
    where (_, p) = head $ filter fst xs