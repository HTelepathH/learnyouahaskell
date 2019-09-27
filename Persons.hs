module Persons
( Person(..)
, Car(..)
) where

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read) 

data Car = Car {company :: String
                , model :: String
                , year :: Int
                } deriving (Show)  

-- Car a b c || type constructor

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

type AssocList k v = [(k,v)] 
--[(1,2),(3,5),(8,9)] :: AssocList Int Int
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)] 

type IntMap v = Map Int v 
type IntMap = Map Int 

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)  
