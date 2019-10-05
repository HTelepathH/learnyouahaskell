import System.IO 
import System.Environment
import System.Directory
import Data.List

dispatch :: [(String, [String] -> IO())]
dispatch = [ ("-a", add)
           , ("-v", view)
           , ("-rm", remove)
           , ("-bump", bump)
           ]


main = do
    (command : args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO()
add [fileName , todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO()
view [fileName] = do
    contents <- readFile fileName
    let todoes = lines contents
    mapM_ putStrLn $ zipWith (\n line -> show n ++ " - " ++ line) [0..] todoes
    
remove :: [String] -> IO()
remove [fileName , numberString] = do
    contents <- readFile fileName
    (tempName, tempHandle) <- openTempFile "." "temp"
    let tasks = lines contents
        i = read numberString
        newtasks = delete (tasks !! i) tasks
    hPutStr tempHandle $ unlines newtasks
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

bump :: [String] -> IO()
bump [fileName, numberString] = do
    contents <- readFile fileName
    (tempName, tempHandle) <- openTempFile "." "temp"
    let tasks = lines contents
        i = read numberString
        bumpItem = tasks !! i
        newtasks = bumpItem : (delete (tasks !! i) tasks)
    hPutStr tempHandle $ unlines newtasks
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName