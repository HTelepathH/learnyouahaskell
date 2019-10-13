import Data.Char  
import Control.Monad   

{- 
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main 
-}

{- 

main = do  
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main
    --perform the I/O action getLine(return) and then bind its result value to name

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-} 


main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?" 
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: " 
    mapM putStrLn colors