import Data.List

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = Folder "root"   
            [ File "goat_yelling_like_man.wmv" "baaaaaa"  
            , File "pope_time.avi" "god bless"  
            , Folder "pics"  
                [ File "ape_throwing_up.jpg" "bleargh"  
                , File "watermelon_smash.gif" "smash!!"  
                , File "skull_man(scary).bmp" "Yikes!"  
                ]  
            , File "dijon_poupon.doc" "best mustard"  
            , Folder "programs"  
                [ File "fartwizard.exe" "10gotofart"  
                , File "owl_bandit.dmg" "mov eax, h00t"  
                , File "not_a_virus.exe" "really not a virus"  
                , Folder "source code"  
                    [ File "best_hs_prog.hs" "main = print (fix error)"  
                    , File "random.hs" "main = print 4"  
                    ]  
                ]  
            ]  

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (focus, FSCrumb name l r : bs) = Just (Folder name (l ++ [focus] ++ r), bs)
fsUp (_, []) = Nothing

fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo name (Folder fname items, bs) = 
    case break (nameIs name) items of (l, cur:r) -> Just (cur, FSCrumb fname l r : bs)
                                      (_, [])    -> Nothing 

nameIs :: Name -> FSItem -> Bool
nameIs name (File n _) = name == n
nameIs name (Folder n _) = name == n 

fsRename :: Name -> FSZipper -> Maybe FSZipper
fsRename newName (Folder name items, bs) = Just (Folder newName items, bs)  
fsRename newName (File name dat, bs) = Just (File newName dat, bs) 

{-  Just fst <*> (return (myDisk,[]) >>= fsTo "pics" >>= fsTo "skull_man(scary).bmp") -}