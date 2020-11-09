module Test where

import Data.Char

main = putStrLn "hello world"

lenVec3 a b c = sqrt (a^2 + b^2 + c^2)

sign x = if x == 0 then 0 else (if x > 0 then 1 else (-1))

infixl 6 |-|

minus x y = x - y

(|-|) x y = if x - y >= 0 then x - y else - (x - y)

-- logBase 4 $ min 20 $ 9 + 7

low x = toLower x

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then 10 * digitToInt x + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact (-1) = 1
doubleFact n = n * doubleFact (n - 2)

fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)


-- | n < 0 = if mod n 2 == 1 then fibonacci (n + 1) + fibonacci (n + 2) else - (fibonacci (n + 1) + fibonacci (n + 2))
test n = n < 0 && mod n 2 == 1

fibonacci2 n  | n == 0 = 0
              | n == 1 = 1
              | n > 0 = helper' n 1 0 1
              | n < 0 = helper'' (-n) 1 0 1

helper' n i k' k'' = if n == i then k'' else helper' n (i + 1) k'' (k' + k'')
helper''  n i k' k'' = if n == i then k'' else helper'' n (i + 1) k'' (k' - k'')

seqA :: Integer -> Integer
seqA n 
  | n == 0 = 1
  | n == 1 = 2
  | n == 2 = 3
  | otherwise = let
      helper k1 k2 k3 2 = k3
      helper k1 k2 k3 n = helper k2 k3 (k2 + k3 - (2 * k1)) (n-1)
    in helper 1 2 3 n


-- sum'n'count :: Integer -> (Integer, Integer)
-- sum'n'count x
--   | x == 0 = (0, 1)
--   | otherwise =
--     let

--       x' = if x >= 0 then x else (-x)

--       digs 0 = []
--       digs x = digs (x `div` 10) ++ [x `mod` 10]

--       length' x = sum [1 | _ <- x]

--       s = sum (digs x')
--       c = length' (digs x')
--     in (s, c)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper 0 0 (abs x)
    where
        helper 0 0 0       = (0, 1)
        helper sum count 0 = (sum, count)
        helper sum count value = helper (sum + mod value 10 ) (count + 1) (div value 10)



integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b = helper f a b 5000 0
  where
    helper f a b 0 s = s
    helper f a b c s = helper f (a + ((b - a) / c))  b (c - 1) (s + (((b - a) / c) * f a))


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper f a b 50000 a 0
  where
    helper f a b 0 i s = ((b - a) / 50000) * (((f a) + (f b) / 2) + s)
    helper f a b c i s = helper f a  b (c - 1) (i + ((b - i) / c)) (s + (f i))

integration'' :: (Double -> Double) -> Double -> Double -> Double
integration'' f a b = h * ((f a + f b) / 2 + sumf 1 0) where
    n = 1000
    h = (b - a) / n
    sumf i acc | i == n = acc
               | otherwise = sumf (i + 1) (acc + f (a + (h * i)))


getSecondFrom :: t -> t1 -> t2 -> t1
getSecondFrom a b c = b





