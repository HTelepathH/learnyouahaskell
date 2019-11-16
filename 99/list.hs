import Data.List
import Data.Function (on)

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == (reverse x)

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = foldr (++) [] $ map flatten xs

compress :: Eq a => [a] -> [a]
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys

pack :: Eq a => [a] -> [[a]]
pack xs = make [] xs
    where 
        make ch (x:ys@(y:_))
            | x == y    = make (x:ch) ys
            | otherwise = (x:ch) : make [] ys
        make ch (x:[]) = [x:ch]
        make _ [] = []

pack' :: Eq a => [a] -> [[a]]    
pack' (x:xs) = let (first,rest) = span (==x) xs
          in (x:first) : pack rest
pack' [] = []

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

data Elem a = Multiple Int a | Single a deriving (Show)
encodeModified :: Eq a => [a] -> [Elem a]
encodeModified = map make . encode
    where make (1, x) = Single x
          make (n, x) = Multiple n x

decodeModified :: Eq a => [Elem a] -> [a]
decodeModified = concatMap f 
    where f (Single x) = [x]
          f (Multiple n x) = replicate n x

encodeDirect :: Eq a => [a] -> [Elem a]
encodeDirect  = map make . encode'
    where make (1, x) = Single x
          make (n, x) = Multiple n x

encode' :: Eq a => [a] -> [(Int, a)]
encode' = foldr helper []
    where helper x [] = [(1,x)]
          helper x (y@(a, b):xs)
            | x == b    = (a+1, b):xs
            | otherwise = (1, x):y:xs

dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

repli :: [a] -> Int -> [a]
repli xs n = foldr (\x acc -> make n x acc) [] xs
        where make 0 _ acc = acc
              make n x acc = make (n-1) x (x:acc)

dropEvery :: [a] -> Int -> [a]
dropEvery a n = helper n 1 a
    where helper n cnt (x:xs)
            | n == cnt  = helper n 1 xs
            | otherwise = x : helper n (cnt+1) xs
          helper _ _ [] = []

split = flip splitAt

slice :: [a] -> Int -> Int -> [a]
slice x b e =  drop (b-1) . take e $ x

rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) $ drop (length xs + n) $ cycle xs

removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = case back of
        [] -> error "removeAt: index too large"
        x:rest -> (x, front ++ rest)
  where (front, back) = splitAt (k - 1) xs

range :: Int -> Int -> [Int]
range b e = [b..e]
{- 
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)
 -}

combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 x  = []
combinations n (x:ys) = map (x:) (combinations (n-1) ys) ++ combinations n ys

combo :: Int -> [a] -> [([a], [a])]
combo 0 xs     = [([], xs)]
combo _ []     = []
combo n (x:xs) = takes ++ ntakes
    where
        takes  = [ (x:as, bs) | (as,bs) <- combo (n-1) xs]
        ntakes = [ (as, x:bs) | (as,bs) <- combo n xs]

group' :: [Int] -> [a] -> [[[a]]]
group' [] _ = [[]]
group' (n:ns) xs = [ts : rest | (ts, nts) <- combo n xs, rest <- group' ns nts]

lsort :: [[a]] -> [[a]]
lsort = sortOn length

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort


isPrime :: Integral a => a -> Bool
isPrime k = k > 1 &&
   foldr (\p r -> p*p > k || k `rem` p /= 0 && r)
      True primesTME

primesTME :: Integral a => [a]
primesTME = 2 : gaps 3 (join [[p*p, p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p, p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

gcdd :: Int -> Int -> Int
gcdd a b
    | b == 0    = a
    | otherwise = gcdd b (a `mod` b)

goldBach :: Int -> (Int, Int)
goldBach a = head $
                     filter (\(x,y) -> isPrime x && isPrime y) $
                     map (\e -> (e, a - e)) (2:[3, 5..a `div` 2])
 where
    factors a = filter (isFactor a) [2..a-1]
    isFactor a b = a `mod` b == 0
    isPrime a = null $ factors a


goldbachList a b = map goldBach $ dropWhile (<4) $ filter even [a..b]
goldbachList' a b i = filter (\(x, y) -> x > i && y > i) $ goldbachList a b