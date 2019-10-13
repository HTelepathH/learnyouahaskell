import System.IO 
import Data.Char

main = do  
    contents <- readFile "gf.txt"  
    putStr contents 
    writeFile "gf_caps.txt" $ map toUpper contents

{-

main = do     
    withFile "gf.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents) 

-}

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result