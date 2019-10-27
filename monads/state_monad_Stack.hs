import Control.Monad.State

{- 
newtype State s a = State { runState :: s -> (a, s)}

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in  g newState

    :: (Monad s->(a,s))   a->(Monad s->(b,s))   Monad s->(b,s)
    :: (State s a) -> (a->State s b) -> (State s b) 

{- runState (join' (state $ \s -> (push 10,1:2:s))) [0,0,0] -}
join' :: (Monad m) => m (m a) -> m a
join' mm = mm >>= (\m -> m)
-}

type Stack = [Int]

pop_ :: Stack -> (Int, Stack)
pop_ (x:xs) = (x, xs)

push_ :: Int -> Stack -> ((), Stack)
push_ x st = ((), x:st)

stackManip_ :: Stack -> (Int, Stack)
stackManip_ stack = let
    ((), st1) = push_ 3 stack
    (_, st2) = pop_ st1
    in pop_ st2

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \x -> ((), a:x)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

stackp :: State Stack Int
stackp = return () >>= (\_ -> push 3 >>= (\_ -> pop >>= (\_ -> pop)))

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()  
{- 
get = State $ \s -> (s, s)
put newState = State $ \s -> ((), newState)
 -}
 
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1, 2, 3]
        then put [8, 3, 1]
        else put [9, 2, 1]

{- runState (join' (state $ \s -> (push 10,1:2:s))) [0,0,0] -}
join' :: (Monad m) => m (m a) -> m a
join' mm = mm >>= (\m -> m)