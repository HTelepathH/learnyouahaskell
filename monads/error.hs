import Control.Monad.Error

instance (Error e) => Monad (Either e) where
    return x = Right x
    Right x >>= f = f x
    Left er >>= _ = Left er
    fail msg = Left (strMsg msg) 

Right 3 >>= \x -> return (x + 100)

Left "boom" >>= \x -> return (x+1)
Right 100 >>= \x -> Left "no way!"
Right 3 >>= \x -> return (x + 100)