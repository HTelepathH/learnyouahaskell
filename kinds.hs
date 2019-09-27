class Tofu t where  
    tofu :: j a -> t a j  
-- :k Tofu
--  * -> (* -> *) -> *
data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu = Frank

data Barry t k p = Barry { yabba :: p, dabba :: t k }  
-- k, p = *
-- t = (* -> *)

-- Functor (a -> b) -> f a -> f b
instance Functor (Barry t k) where
    fmap f (Barry x y) = Barry (f x) y