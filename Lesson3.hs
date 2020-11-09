module Lesson3 where

import Data.Char

import Data.List

addTwoElements :: a -> a -> [a] -> [a]
-- addTwoElements a b c = a : b : c
addTwoElements a b = (a :) . (b :)


nTimes:: a -> Int -> [a]
nTimes a c = helper a c []
  where
    helper a 0 l = l
    helper a c l = helper a (c - 1) (a : l)

nTimes':: a -> Int -> [a]
nTimes' x n
  | n == 0 = []
  | otherwise = x : nTimes x (n - 1)


-- sndHead ((_,x): _) = x

-- sndHead ((:) ((,) _ x) y) = x

-- sndHead ((,) y x : z) = x

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) = if x `mod` 2 == 0 then oddsOnly xs else x : oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = helper l (reverse l)
  where
    helper [] [] = True
    helper (x : xs) (y : ys)
      | (xs == []) && (ys == []) = True
      | x == y = helper xs ys
      | otherwise = False


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 a b c = helper a b c []
  where
    helper (a : as) (b : bs) (c : cs) r = helper as bs cs (a + b + c : r)
    helper [] (b : bs) (c : cs) r = helper [] bs cs (b + c : r)
    helper (a : as) [] (c : cs) r = helper as [] cs (a + c : r)
    helper (a : as) (b : bs) [] r = helper as [] bs (a + b : r)
    helper (a : as) [] [] r = helper as [] [] (a : r)
    helper [] (b : bs) [] r = helper [] bs [] (b : r)
    helper [] [] (c : cs) r = helper [] [] cs (c : r)
    helper [] [] [] r = reverse r


sum3' :: Num a => [a] -> [a] -> [a] -> [a]
sum3' xs ys zs = xs `sum2` ys `sum2` zs
  where
    sum2 [] bs = bs
    sum2 as [] = as
    sum2 (a : as) (b : bs) = (a + b) : sum2 as bs


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems l = helper l [[]]
  where
    helper (x : xs) (y : ys)
      | y == [] = helper xs ([x] : ys)
      | x /= head y = helper xs ([x] : y : ys)
      | otherwise = helper xs ((x : y) : ys)
    helper [] r = reverse r


readDigits' :: String -> (String, String)
readDigits' [] = ([], [])
readDigits' (x : xs)
  | isDigit x = (x : fst (readDigits xs), snd (readDigits xs))
  | otherwise = (fst (readDigits xs), x : snd (readDigits xs))



readDigits :: String -> (String, String)
readDigits = span isDigit


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x : xs)
  | p1 x || p2 x = x : filterDisj p1 p2 xs
  | otherwise = filterDisj p1 p2 xs


filterDisj' :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj' p1 p2 = filter (\x -> p1 x || p2 x)



divideList :: Ord a => a -> [a] -> ([a], [a])
divideList _ [] = ([], [])
divideList a (x : xs)
  | x > a = (x : fst (divideList a xs), snd (divideList a xs))
  | otherwise = (fst (divideList a xs), x : snd (divideList a xs))



qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = 
  let
    divideList :: Ord a => a -> [a] -> ([a], [a])
    divideList _ [] = ([], [])
    divideList a (x : xs)
      | x > a = (x : fst (divideList a xs), snd (divideList a xs))
      | otherwise = (fst (divideList a xs), x : snd (divideList a xs))
    (gt, lt) = divideList x xs
  in (qsort lt) ++ (x : (qsort gt))


qsort' :: Ord a => [a] -> [a]
qsort' (x:xs) = qsort (filter (<x) xs) ++ (x : qsort (filter (>=x) xs))
qsort' [] = []


dfilter :: (a -> Bool) -> [a] -> ([a], [a])
dfilter _ [] = ([], [])
dfilter p (x:xs) = let
                      (a, b) = dfilter p xs
                   in case p x of True  -> (x : a, b)
                                  False -> (a, x : b)


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])


perm' :: (Eq n, Num n) => n -> [a] -> [[a]]
perm' 0 _ = []
perm' n (x : xs) = [x : xs, x : reverse xs] ++ perm' (n - 1) (xs ++ [x])

perms' :: Eq a => [a] -> [[a]]
perms' (x : [y]) = [[x, y],[y, x]]
perms' x = concatMap (helper x) x
  where
    helper l x = map ((\x y -> x : y) x) (perms' (filter (/=x) l))


perms'' :: [a] -> [[a]]
perms'' (x : [y]) = [[x, y],[y, x]]
perms'' l @ (x : xs) = concatMap (helper x xs) l
  where
    helper x xs _ = map ((\x y -> x : y) x) (perms (xs))


rearr :: [a] -> [[a]]
rearr [] = []
rearr (x : xs) = (xs ++ [x])  : rearr (xs)


rotations :: [a] -> [[a]]
rotations xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap (rotations.(x:)) (perms xs)


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> x `max` y `max` z)


fibStream :: [Integer]
fibStream = zipWith (+) (fib (-1) 1) (fib 1 0)
  where
    fib i j = i : fib j (i + j)


fibStream' :: [Integer]
fibStream' = 0 : zipWith (+) fibStream (1 : fibStream)


data Odd = Odd Integer 
  deriving (Eq, Show)


oddValue :: Odd -> Integer
oddValue (Odd x) = x

instance Enum Odd where
  toEnum = Odd . fromIntegral
  fromEnum (Odd a) = fromIntegral a
  succ (Odd a) = Odd $ a + 2
  pred (Odd a) = Odd $ a - 2
  enumFrom (Odd a) = map Odd [a, a+2 .. ]
  enumFromThen (Odd x) (Odd y) = map Odd [x, y .. ]
  enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x, y .. z]


coins ::(Ord a, Num a) => [a]
coins = [2,3,7]
 
change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change amount = [ c:cs |c<-coins, amount>=c, cs<-change (amount - c) ]


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f init [] = init
foldr' f init (x : xs) = x `f` foldr' f init xs


lengthList :: [a] -> Integer
lengthList = foldr' (\_ s -> 1 + s) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

sumOdd' :: [Integer] -> Integer
sumOdd' = foldr (\x s -> (x `mod` 2) * x + s) 0

meanList :: [Double] -> Double
meanList = (\(s,c) -> s/c) . foldr (\x (s,c) -> (x + s, 1 + c)) (0,0)

meanList' :: [Double] -> Double
meanList' = (uncurry (/)) . (foldr (\x (s, cnt) -> (s + x, cnt + 1)) (0, 0))


evenOnly :: [a] -> [a]
evenOnly = fst . foldl (\(xs,c) x -> (if odd c then xs ++ [x] else xs, c + 1)) ([], 0)


evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\a (xs, ys) -> (a : ys, xs)) ([], [])


evenOnly'' :: [a] -> [a]
evenOnly'' = snd . foldr (\x ~(xs, ys) -> (x : ys, xs)) ([], [])

lastElem :: [a] -> a
lastElem = foldl1 (flip const)


-- unfold :: (t -> (a, t)) -> t -> [a]
-- unfold f ini = let (x, ini') = f ini in
--   x : unfold f ini'

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\(a,z) -> if a > z then Nothing else Just (z, (a,pred z)))







