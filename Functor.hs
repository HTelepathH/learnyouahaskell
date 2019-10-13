import BinaryTree as B
import qualified Data.Map as M

-- Functor : can be mapped over
-- a type constructor in implementation

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' (Either a) where  
    fmap' f (Right x) = Right (f x)  
    fmap' f (Left x) = Left x  

instance Functor' Maybe where  
    fmap' f (Just x) = Just (f x)  
    fmap' f Nothing = Nothing  

instance Functor' Tree where
    fmap' f (Node a left right) = Node (f a) (fmap' f left) (fmap' f right)
    fmap' f EmptyTree = EmptyTree

instance (Ord k) => Functor' (M.Map k) where
  --  fmap' f (M.empty) = M.empty
    fmap' f m = let l = M.toList m in 
                M.fromList $ foldr (\(k, v) acc -> (k, f v) : acc) [] l