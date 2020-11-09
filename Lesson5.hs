module Lesson5 where

import Control.Monad.Writer as W
import Control.Monad.State as S
import Control.Monad.Reader as R

import Data.Char (toUpper, isDigit, digitToInt)
import System.Directory
import Data.List (isInfixOf)
import Data.Monoid

data Point3D a = Point3D a a a deriving Show


instance Functor Point3D where
  fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show

instance Functor GeomPrimitive where
  fmap f (Point x) = Point (fmap f x)
  fmap f (LineSegment x y) = LineSegment (fmap f x) (fmap f y)


-- data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

-- instance Functor Tree where
--   fmap _ (Leaf Nothing) = Leaf Nothing
--   fmap f (Leaf (Just x)) = Leaf (Just $ f x)
--   fmap f (Branch l (Just x) r) = Branch (fmap f l) (Just $ f x) (fmap f r) 
--   fmap f (Branch l _ r) = Branch (fmap f l) Nothing (fmap f r) 


-- instance Functor Tree where
--     fmap f (Leaf a) = Leaf (fmap f a)
--     fmap f (Branch a b c) = Branch (fmap f a) (fmap f b) (fmap f c)


data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry x v) = Entry x (f v)


instance Functor (Map k1 k2) where
  fmap _ (Map []) = Map []
  fmap f (Map (Entry x v : xs)) = let Map ys = fmap f (Map xs) in Map ((Entry x (f v)) : ys)


-- instance Functor (Map k1 k2) where
--    fmap f (Map m) = Map ( map (fmap f) m )

-- instance Functor (Map k1 k2) where
--     fmap f (Map es) = Map [fmap f e | e <- es]

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s = Log [s] . f

-- execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
-- execLoggers a f g = let Log msg res = f a in let Log msg' res' = g res in Log (msg ++ msg') res'

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (str1 ++ str2 ) x2    where
    Log str1 x1 = f x
    Log str2 x2 = g x1


-- execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
-- execLoggers x f g = let
--         Log [m1] y = f x
--         Log [m2] z = g y
--     in
--         Log [m1,m2] z

returnLog :: a -> Log a
returnLog = Log []

add1Log :: Num a => a -> Log a
add1Log = toLogger (+1) "added one"

mult2Log :: Num a => a -> Log a
mult2Log = toLogger (* 2) "multiplied by 2"


bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg a) f = let Log msg' b = f a in Log (msg ++ msg') b

-- instance Functor Log where
--   fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x (f : fs) = foldl bindLog (f x) fs

-- execLoggersList = foldl (>>=) . return

data SomeType a = A

instance Functor Log where
    fmap f x = x >>= return . f
    -- fmap = (=<<) . (return .)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken [] = Nothing
asToken str
  | str == "+" = Just (Plus)
  | str == "-" = Just (Minus)
  | str == "(" = Just (LeftBrace)
  | str == ")" = Just (RightBrace)
  | otherwise = if all isDigit str then Just (Number $ read str) else Nothing

tokenize :: String -> Maybe [Token]
-- tokenize = sequence . map asToken . words
tokenize = cvt . words where
  cvt :: [String] -> Maybe [Token]
  cvt [] = Just []
  cvt (w:ws) = do
    t <- asToken w -- Token
    ts <- cvt ws -- [Token]
    return (t:ts)


data Board = Board Int deriving Show

nextPositions :: Board -> [Board]
nextPositions (Board x) = map Board [x-1,x+1] 

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred
  | n < 0 = []
  | n == 0 = if pred b then [b] else []
  | otherwise = do
      nextBoard <- nextPositions b
      nextBoards' <- nextPositionsN nextBoard (n-1) pred
      return nextBoards'

pred' :: Board -> Bool
pred' (Board x) = x < 100


makeList :: Int -> [(Int, Int)]
makeList x = do
  x <- list x
  y <- list'
  return (x, y)

list :: Int -> [Int]
list x = [x,x,x] 

list' :: [Int]
list' = [4,5,6]

makeList' :: [(Int, Int)]
makeList' = [1,2,3] >>= (\x -> [4,5,6] >>= (\y -> return (x, y)))


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x
  | x <= 0 = []
  | otherwise = do
      c <- enumFromTo 1 x
      b <- enumFromTo 1 x
      a <- enumFromTo 1 x
      if a^2 + b^2 == c^2 && a < b then "z" else []
      return (a,b,c)

-- pythagoreanTriple x = do
--         c <- [1..x]
--         b <- [1..(c-1)]
--         a <- [1..(b-1)]
--         True <- return $ a^2 + b^2 == c^2
--         return (a,b,c)

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if length name >= 1 then putStrLn ("Hi, " ++ name ++ "!") else main'

main'' :: IO ()
main'' = do
  putStr "Substring: "
  substr <- getLine
  if length substr == 0 then putStrLn "Canceled" else do
  dir <- getDirectoryContents "."
  mapM_ (\x -> if substr `isInfixOf` x then putStrLn ("Removing file: " ++ x) >> removeFile x else return ()) dir

-- data Reader r a = Reader { runReader :: (r -> a) }
type User = String
type Password = String
type UsersTable = [(User,Password)]

pwds :: UsersTable
pwds = [("Bill","123"), ("Ann","qwert"), ("John","123")]

-- instance Monad (Reader r) where
--   return x = Reader $ \_ -> x
--   m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

-- local' :: (r -> r') -> Reader r' a -> Reader r a
-- local' f m = Reader $ \e -> runReader m (f e)

-- instance Applicative (Reader r) where
--   pure = return
--   (<*>) = ap

-- instance Functor (Reader r) where
--     fmap f x = x >>= return . f

-- ask = Reader id

-- fstUser :: Reader UsersTable User
-- fstUser = do
--   e <- ask
--   return $ fst (head e)

-- usersWithBadPasswords :: Reader UsersTable [User]
-- usersWithBadPasswords = do
--   e <- ask
--   return $ helper e
--     where
--       helper :: UsersTable -> [User]
--       helper [] = []
--       helper ((name, psw) : xs) = if "123456" == psw then name : helper xs else helper xs

-- usersWithBadPasswords = asks $ \xs -> [x | (x, "123456") <- xs]
-- usersWithBadPasswords = asks (map fst . filter ((==) "123456" . snd))

newtype Writer w a = Writer {runWriter :: (a,w)}

-- instance (Monoid w) => Monad (Writer w) where
--   return x = Writer (x, mempty)
--   m >>= k =
--     let (x,u) = runWriter m
--         (y,v) = runWriter $ k x
--     in Writer (y, u `mappend` v)

-- execWriter :: Writer w a -> w
-- execWriter = snd. runWriter

-- evalWriter :: Writer w a -> a
-- evalWriter = fst . runWriter

-- instance Applicative (Writer w) where
--   pure = return
--   (<*>) = ap

-- instance Functor (Writer w) where
--     fmap f x = x >>= return . f


type Shopping = W.Writer (Sum Integer, [String]) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328

-- tell :: Monoid w => w -> W.Writer w ()
-- tell w = W.writer ((), w)

purchase :: String -> Integer -> Shopping
purchase item cost = W.writer ((), (Sum cost, [item]))

total :: Shopping -> Integer
total x = case W.execWriter x of (Sum log, _) -> log

items :: Shopping -> [String]
items x = case W.execWriter x of (_, purchases) -> purchases

-- purchase :: String -> Integer -> Shopping
-- purchase item cost = tell ([item], Sum cost)

-- total :: Shopping -> Integer
-- total = getSum . snd . execWriter

-- items :: Shopping -> [String]
-- items = fst . execWriter


-- purchase :: String -> Integer -> Shopping
-- purchase _ = W.tell . Sum

-- total :: Shopping -> Integer
-- total = getSum . execWriter

readerToState :: R.Reader r a -> State r a
readerToState m = S.state $ (\x -> (R.runReader m x, x))

-- readerToState m = do 
--    st <- get 
--    return $ runReader m st


writerToState :: Monoid w => W.Writer w a -> State w a
writerToState m = S.state $ \x -> (fst (W.runWriter m), x <> snd (W.runWriter m))

-- writerToState m = State $ \s -> let (v, l) = runWriter m in (v, s `mappend` l)


fibStep :: State (Integer, Integer) ()
fibStep = do
  (a,b) <- get
  put (b, a + b)

-- fibStep = modify $ \(a,b) -> (b, a + b)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM_ n m


data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (tickTree tree) 1


nTree :: State Integer Integer -> Tree () -> Tree Integer
nTree s (Leaf ()) = Leaf (evalState s 5)

nTree s (Fork a _ c) = Fork (nTree s a) 2 (nTree s c)


tick :: State Integer Integer
tick = do
  c <- get
  put (c + 1)
  return c

-- plus :: Int -> Int -> Int
-- plus n x = execState (sequence $ replicate n tick) x

-- getTick :: State Int Int -> 

tickTree :: Tree () -> State Integer (Tree Integer)
tickTree (Leaf ()) = do
  c <- get
  put (c + 1)
  return (Leaf c)

tickTree (Fork a _ c) = do
  ta <- tickTree a
  x <- get
  put (x + 1)
  tc <- tickTree c
  return (Fork ta x tc)
  
  









