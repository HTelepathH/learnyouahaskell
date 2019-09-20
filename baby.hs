head' :: [a] -> a
head' xs = case xs of [] -> error "empty list"
                      (x:_) -> x

tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' [x] = []
tail' (_:xs) = xs

sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

bmi :: (RealFloat a) => a -> a -> String
bmi w h  
    | k <= skinny = "underweight"
    | k <= 25.0 = "normal"
    | k <= 30 =  "overweight"
    | otherwise = "BIG"
    where k = w / h ^ 2 
          skinny = 18.5

initial :: String -> String -> String
initial first last = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = first -- notice pattern match on left
          (l:_) = last

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' xs = foldl1 (max) xs

-- bubble sort
swap :: (Ord a) => [a] -> [a] 
swap [] = []
swap [x] = [x]
swap (x:xs:t)
    | x > xs = xs : swap (x:t)
    | otherwise = x : swap (xs:t) 

bubble :: (Ord a) => [a] -> [a]
bubble [] = []
bubble xs
    | xs == swap xs = xs
    | otherwise = bubble (init l) ++ [last l]
        where l = swap xs

-- quick sort
qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = qs (filter (<= x) xs) ++ [x] ++ qs (filter (> x) xs) 

rept' :: (Ord n, Num n) => n -> a -> [a]
rept' n k
    | n <= 0 = []
    | otherwise = k : rept' (n-1) k

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' i _
    | i <=0 = [] -- i > 0 fall through
take' _ [] = []
take' i (x:xs) = x : take' (i-1) xs

rev' :: [a] -> [a]
rev' = foldl (\acc x -> x : acc) []

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = [(a,b)] ++ zip' as bs

elem' :: Eq a => a -> [a] -> Bool
elem' a = foldl (\acc x -> if x == a then True else acc) False

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

flip' :: (a -> b -> c) -> (b -> a -> c)
-- curring (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
--or flip' f = \x y -> f y x

div3829 :: (Integral a) => a
div3829 = head $ filter (\a -> mod a 3829 == 0) [100000, 99999..]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
-- map' f = foldl (\acc x -> acc ++ [f x]) []

{-
tails'                   :: [a] -> [[a]]
tails' lst               =  build (\c n ->
  let tailsGo xs = xs `c` case xs of
                             []      -> n
                             _ : xs' -> tailsGo xs'
  in tailsGo lst)
-}

-- Map
findKey :: Eq k => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing  
findKey key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey key xs  
-- findKey key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing