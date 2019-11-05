import Data.Monoid
{- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids -}

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m 
    mconcat = foldr mappend mempty

{- 
1. mempty 'mappend' x = x
2. x 'mappend' mempty = x
3. (x 'mappend' y) 'mappend' z = x 'mappend' (y 'mappend' z)
 -}

{- lists -}
instance Monoid [a] where
    mempty = []
    mappend = (++)

{- Product -}
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)  

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

{- Bool -}
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)

{- Ordering -}
instance Monoid Ordering where
    mempty = EQ 
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

{- Maybe with Monoid content -}
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2) 

{- Maybe with any content , also Last -}
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)  

instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  


  