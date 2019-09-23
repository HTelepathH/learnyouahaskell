-- an instance of a typeclass means
-- can use the functions that the typeclass defines with that type

{-
standard

class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)  

class (Eq a) => Num a where 
subclass

-}
data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- Show : a -> String
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

-- YesNo type
class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True  

yesnoIf :: YesNo y => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult 
                                      else noResult

