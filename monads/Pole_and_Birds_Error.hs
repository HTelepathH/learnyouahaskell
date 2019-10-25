import Control.Monad.Except

type Birds = Int
type Pole = (Birds, Birds)

showp :: Pole -> String
showp (l, r) = "(" ++  show l ++ ", " ++ show r ++ ")"

landLeft :: Birds -> Pole -> Either String Pole  
landLeft n (left,right) 
    | abs ((left + n) - right) < 4 = Right (left + n, right)  
    | otherwise                    = Left $ showp (left + n,right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)  
    | abs (left - (right + n)) < 4 = Right (left, right + n)  
    | otherwise                    = Left $ showp (left,right + n)

banana :: Pole -> Either String Pole  
banana (left,right) = Left $ showp (left,right) ++ " banana"

routine :: Either String Pole
routine = do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second