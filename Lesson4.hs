module Lesson4 where
import Data.Char
import Data.Time.Clock
import Data.Time.Format
-- import System.Locale
-- import Data.List
import Data.Text (strip)

import Prelude hiding (lookup)
import qualified Data.List as L

data Color = Red | Green | Blue deriving Read

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

stringToColor' :: String -> Color
stringToColor' = read


emptyOrSingleton :: Bool -> a -> [a]
emptyOrSingleton False _ = []
emptyOrSingleton True x = [x]

isEqual :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
isEqual (a, b) (a', b') = a == a' && b == b'

-- data LogLevel = Error | Warning | Info deriving Eq

-- instance Ord LogLevel where
--     compare Info Warning = LT
--     compare Warning Error = LT
--     compare Info Error = LT
--     compare Error Warning = GT
--     compare Warning Info = GT
--     compare Error Info = GT
--     compare Error Error = EQ
--     compare Info Info = EQ
--     compare Warning Warning = EQ

-- cmp :: LogLevel -> LogLevel -> Ordering
-- cmp Info Warning = LT
-- cmp Warning Error = LT
-- cmp Info Error = LT
-- cmp Error Warning = GT
-- cmp Warning Info = GT
-- cmp Error Info = GT
-- cmp Error Error = EQ
-- cmp Info Info = EQ
-- cmp Warning Warning = EQ



-- processData :: SomeData -> String
-- processData x =
--     case doSomeWork x of
--         (Fail, n) -> "Fail: " ++ show n
--         (Success, _) -> "Success"


data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

-- distance :: Point -> Point -> Double
-- distance (Point a b) (Point c d) = sqrt ((c - a) ^ 2 + (d - b) ^ 2)

data Shape = Circle Double | Rectangle Double Double deriving Show

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b


-- data Result' = Fail Int | Success

-- instance Show Result' where
--     show Success = "Success"
--     show (Fail x) = "Fail: " ++ show x

-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' x =
--     case doSomeWork x of
--         (Fail, n) -> Fail n
--         (Success, _) -> Success


-- data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False


-- data Bit = Zero | One
-- data Sign = Minus | Plus
-- data Z = Z Sign [Bit]

-- z2i (Z sign ds) =
--    let u = case sign of { Minus -> -1; Plus -> 1 }
--        f Zero s = s * 2
--        f One s = s * 2 + 1
--        a = foldr f 0 ds
--    in u * a

-- i2z x = 
--    let (u, a) = if x < 0 then (Minus, -x) else (Plus, x)
--        f 0 = []
--        f x = (x `mod` 2) : f (x `div` 2) 
--        g 0 = Zero
--        g 1 = One
--    in Z u (map g (f a))

-- add :: Z -> Z -> Z
-- add x y = i2z (z2i x + z2i y)

-- mul :: Z -> Z -> Z
-- mul x y = i2z (z2i x * z2i y)


-- foo :: Bool -> Int
-- foo ~True = 1
-- foo False = 0


data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show, Eq)



timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving (Show, Eq)

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString x
        | x == Error = "Error"
        | x == Warning = "Warning"
        | x == Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString x = (timeToString . timestamp $ x) ++ ": " ++ (show . logLevel $ x) ++ ": " ++ message x


updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName $ p1 }

updateLastName' :: Person -> Person -> Person
updateLastName' (Person {lastName = ln}) p2 = p2 {lastName = ln}


isRectangle :: Shape -> Bool
isRectangle Rectangle{} = True
isRectangle _ = False

abbrFirstName :: Person -> Person
abbrFirstName p@(Person { firstName = fn }) = if length fn <= 2 then p else p { firstName = head fn : "." }


data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord a b) (Coord c d) = sqrt ((c - a) ^ 2 + (d - b) ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord a b) (Coord c d) = abs (c - a) + abs (d - b)


getCenter :: Double -> Coord Int -> Coord Double
getCenter x (Coord a b) = Coord ((fromIntegral a * x) + (x / 2)) ((fromIntegral b * x) + (x / 2))

getCell :: Double -> Coord Double -> Coord Int
getCell x (Coord a b) = Coord (floor (a / x) :: Int) (floor (b / x) :: Int)


change :: (t -> a) -> Coord t -> Coord a
change f (Coord a b) = Coord (f a) (f b)

getCenter' :: Double -> Coord Int -> Coord Double
getCenter' w = change $ (w *) . (+ 0.5) . fromIntegral

getCell' :: Double -> Coord Double -> Coord Int
getCell' w = change $ floor . (/ w)


findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs) = if isDigit x then Just x else findDigit xs


findDigitOrX :: [Char] -> Char
findDigitOrX x =
    case findDigit x of
        Nothing -> 'X'
        Just x -> x


maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList _ = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x


data Error = ParsingError | IncompleteDataError | IncorrectDataError String

-- parsePerson :: String -> Either Error Person
-- parsePerson s = makePerson (lineWith "firstName ") (lineWith "lastName ") (lineWith "age ")
--   where
--     info :: [(String, String)]
--     info = map (break (== '=')) . lines $ s

--     lineWith :: String -> Maybe String
--     lineWith = flip lookup info

--     makePerson :: Maybe String -> Maybe String -> Maybe String -> Either Error Person
--     makePerson (Just firstNameA) (Just lastNameA) (Just ageA) =
--       case (firstNameA, lastNameA, ageA) of
--         ('=' : ' ' : firstName, '=' : ' ' : lastName, '=' : ' ' : age) ->
--           case reads age of
--             [(i, "")] -> Right $ Person firstName lastName i
--             _         -> Left $ IncorrectDataError age
--         _ -> Left ParsingError
--     makePerson _ _ _ = Left IncompleteDataError

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing



data List a = Nil | Cons a (List a) deriving Show

-- fromList :: List a -> [a]
-- fromList Nil = []
-- fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)


data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc (toNat (x - 1))

add' :: Nat -> Nat -> Nat
add' x y = toNat $ (fromNat x + fromNat y)

mul' :: Nat -> Nat -> Nat
mul' x y = toNat $ (fromNat x * fromNat y)

fac'' :: Nat -> Nat
fac'' x = toNat $ fac' $ fromNat x

fac' :: Integer -> Integer
fac' 0 = 1
fac' x = x * fac' (x - 1)


add :: Nat -> Nat -> Nat
add Zero x = x
add (Suc x) y = add x (Suc y) 

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n1@(Suc n) = mul n1 (fac n)


data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node x y) = if height x > height y then (height x) + 1 else (height y) + 1

size :: Tree a -> Int
size (Leaf _) = 1
size (Node x y) = 1 + size x + size y


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1, x)
    go (Node a b) = (size1+size2, sum1+sum2) where
      (size1,sum1) = go a
      (size2,sum2) = go b



-- infixl 6 :+:
-- infixl 7 :*:
-- data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
--     deriving (Show, Eq)

-- expand :: Expr -> Expr
-- expand ((e1 :+: e2) :*: e) =
--   expand (expand e1 :*: expand e) :+:
--   expand (expand e2 :*: expand e)
-- expand (e :*: (e1 :+: e2)) =
--   expand (expand e :*: expand e1) :+:
--   expand (expand e :*: expand e2)
-- expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand (e1 :*: e2) = let
--     ee1 = expand e1
--     ee2 = expand e2
--   in if ee1 == e1 && ee2 == e2 then e1 :*: e2 else expand $ ee1 :*: ee2
-- expand e = e

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand = foldr1 (:+:) . expandList
  where
    expandList :: Expr -> [Expr]
    expandList (Val i)   = [Val i]
    expandList (l :+: r) = expandList l ++ expandList r
    expandList (l :*: r) = [ e1 :*: e2 | e1 <- expandList l, e2 <- expandList r]



type Endo a = a -> a

newtype A = A A


-- newtype Xor = Xor { getXor :: Bool }
--     deriving (Eq,Show)

-- instance Semigroup Xor where

-- instance Monoid Xor where
--     mempty = Xor False
--     -- mappend (Xor a) (Xor b)
--     --   | a == True && b == False = Xor True
--     --   | a == False && b == True = Xor True
--     --   | otherwise = Xor False
--     Xor x `mappend` Xor y = Xor (x /= y)


-- newtype Maybe' a = Maybe' { getMaybe :: Maybe a } deriving (Eq,Show)

-- instance Semigroup (Maybe' a) where

-- instance Monoid a => Monoid (Maybe' a) where
--   mempty = Maybe' $ Just mempty
--   mappend (Maybe' Nothing) _ = Maybe' Nothing
--   mappend _ (Maybe' Nothing) = Maybe' Nothing
--   mappend (Maybe' a) (Maybe' b) = Maybe' (mappend a b)



-- class MapLike m where
--     empty :: m k v
--     lookup :: Ord k => k -> m k v -> Maybe v
--     insert :: Ord k => k -> v -> m k v -> m k v
--     delete :: Ord k => k -> m k v -> m k v
--     fromList :: Ord k => [(k,v)] -> m k v
--     fromList [] = empty
--     fromList ((k,v):xs) = insert k v (fromList xs)

-- newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
--     deriving (Eq,Show)


-- instance MapLike (ListMap) where
--   empty = (ListMap [])

--   lookup _ (ListMap [] ) = Nothing
--   lookup k' (ListMap ((k,v) : xs)) = if k' == k then Just v else lookup k' (ListMap xs)

--   delete _ (ListMap [] ) = ListMap []
--   delete k' (ListMap ((k,v) : xs)) = if k' == k then ListMap xs else let ListMap ys = delete k' (ListMap xs) in ListMap ((k,v) : ys)

--   insert k v (ListMap [] ) = ListMap [(k,v)]
--   insert k' v' (ListMap ((k,v) : xs)) = if k' == k then ListMap ((k,v') : xs) else let ListMap ys = insert k' v' (ListMap xs) in ListMap ((k,v) : ys)


-- instance MapLike ListMap where
--     empty = ListMap []
--     lookup k = L.lookup k . getListMap
--     delete k = ListMap . filter ((/= k) . fst) . getListMap
--     insert k v = ListMap . ((k,v):) . getListMap . delete k



class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }


instance MapLike ArrowMap where
    empty = ArrowMap (\_ -> Nothing)
    lookup k (ArrowMap f) = f k
    delete k (ArrowMap f) = ArrowMap (\x -> if x == k then Nothing else f x)
    insert k v (ArrowMap f) = ArrowMap (\x -> if x == k then Just v else f x) 
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)





