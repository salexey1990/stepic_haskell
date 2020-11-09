module Lesson2 where

import Data.Function
import Data.Typeable

import Data.Set


-- multSecond = g `on` h

-- g = (*)

-- h = snd

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = (op) (f x) (f y) (f z)

doItYourself = f . g . h

f = logBase 2

g = (^3)

h = max 42

class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Eq a, Printable a, Eq b, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"



class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | (doesEnrageGork a && doesEnrageMork a) = (stomp . stab) a
                  | doesEnrageGork a = stab a
                  | doesEnrageMork a = stomp a
                  | otherwise = a

-- a = 127.2
-- b = 24.1
-- c = 20.1
-- d = 2

-- ip = show a ++ show b ++ show c ++ show d

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a | a == maxBound = minBound
          | otherwise = succ a

  spred :: a -> a
  spred a | a == minBound = maxBound
          | otherwise = pred a




getMaxBound :: Bounded a => a -> a
getMaxBound _ = maxBound

getMinBound :: Bounded a => a -> a
getMinBound _ = minBound

instance SafeEnum Bool

avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger ((toInteger a) + (toInteger b) + (toInteger c)) / 3.0 :: Double

foo 0 x = x
foo n x = let x' = foo (n - 1) (x + 1)
          in x' `seq` x'

bar 0 f = f
bar x f = let f' = \a -> f (x + a)
              x' = x - 1
          in f' `seq` x' `seq` bar x' f'

baz 0 (x, y) = x + y
baz n (x, y) = let x' = x + 1
                   y' = y - 1
                   p  = (x', y')
                   n' = n - 1
               in p `seq` n' `seq` baz n' p

quux 0 (x, y) = x + y
quux n (x, y) = let x' = x + 1
                    y' = y - 1
                    p  = (x', y')
                    n' = n - 1
                in x' `seq` y' `seq` n' `seq` quux n' p


mySum acc 0 = acc
mySum (result, ()) n = (mySum $! (result + n, ())) $ n - 1

goSum = mySum (0, ())


