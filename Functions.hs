{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Functions where

import Data.Char
       hiding (isSpace)
import Data.List
       hiding (transpose, lcm)
import Data.Maybe        

isPrime :: Int -> Bool
isPrime x
  = isPrime' x [2..floor(sqrt(fromIntegral x))]
  where
  isPrime' x []     = True
  isPrime' x (m:ms) = not (x `mod` m == 0) && isPrime' x ms 

fact :: Int -> Int
fact x
  | x == 0    = 1
  | otherwise = x * fact (x -1)

perm :: Int -> Int -> Int
perm x r
  | x == r    = 1
  | otherwise = x * perm ( x - 1) r

choose :: Int -> Int -> Int
choose n r
  | n <  r    = 0
  | r == n    = 1
  | otherwise = (choose (n-1) r) * n `div` (n - r)   


remainder :: Int -> Int -> Int
remainder x y
  | x < y     = x
  | otherwise = remainder (x - y) y

quotient :: Int -> Int -> Int
quotient x y
  | x < y     = 0
  | otherwise = 1 + quotient (x - y) y


add :: Int -> Int -> Int
add x y
  | y == 0    = x
  | otherwise = succ (add x (y-1))

larger :: Int -> Int -> Int
larger x y
  | x <= 0 && y <= 0 = 0
  | otherwise        = succ (larger (x - 1 ) ( y - 1))

chop :: Int -> (Int, Int)
chop x
  = chop' x 0 
  where
    chop' x y
      | x < 10    = (y , x)
      | otherwise = chop' (x - 10) (y + 1)


concatenate :: Int -> Int -> Int
concatenate x 0 = x
concatenate 0 y = y
concatenate x y = concatenate x' y'
  where
    x' = fst (chop x)
    y' = snd (chop x) * (10 ^ m) + y 
    m  = floor (logBase 10 (fromIntegral y)) + 1


fib :: Int -> Int
fib x
  = fib' 1 0 0
  where
    fib' k' k n
      | n == x    = k
      | otherwise = fib' (k + k') k' (n + 1)

goldenRatio :: Float -> Float
goldenRatio e
  | e <= 0    = error "Larger input please!"
  | otherwise = goldenRatio' 1 2 4 e
  where
    goldenRatio' :: Float -> Float -> Int -> Float -> Float
    goldenRatio' r' r n m
      | abs (r' - r) < m = r'
      | otherwise        = goldenRatio' r'' r' (n + 1) m
      where
        i'  = fromIntegral (fib n)
        i   = fromIntegral (fib (n - 1))
        r'' = i' / i


precedes :: String -> String -> Bool
precedes "" _ = True
precedes _ "" = False
precedes (x : xs) (m : ms)
  | x < m     = True
  | x > m     = False
  | otherwise = precedes xs ms


pos :: Eq a => a -> [a] -> Int
pos x ms
  = pos' 0 x ms
  where
    pos' n x (m : ms)
      | x == m    = n
      | otherwise = pos' (n + 1) x ms

twoSame :: [Int] -> Bool
twoSame [] = False
twoSame (x : xs)
  | elem x xs = True
  | otherwise = twoSame xs

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = (rev xs) ++ [x] 

rev' :: [a] -> [a]
rev' [] = []
rev' xs
  = rev'' [] xs
  where
    rev'' ms (x : xs)
      | null xs   = x : ms
      | otherwise = rev'' (x : ms) xs

substring :: String -> String -> Bool
substring "" _ = True
substring _ [] = False
substring m a@(x : xs)
  = m == sub || substring m xs
  where
    sub = take (length m) a 

transpose :: String -> String -> String -> String
transpose s s' [] = [] 
transpose s s' (c : cs)
  = s !! (pos c s') : (transpose s s' cs) 

isSpace :: Char -> Bool
isSpace x 
  | x == ' ' || x == '\n' || x == '\t' = True
  | otherwise = False

removeWhitespace :: String -> String
removeWhitespace xs
  = dropWhile (isSpace) xs

nextWord :: String -> (String, String)
--Pre: The first character is non-whitespace
nextWord ""
  = ("", "")
nextWord (c : cs)
  | isSpace c = ("", cs)
  | otherwise = (c : w, s)
  where
    (w, s) = nextWord cs

splitUp :: String -> [String]
splitUp "" = []
splitUp xs 
  =  word : splitUp sentence
  where 
    (word, sentence) = nextWord xs'
    xs' = removeWhitespace xs

primeFactors :: Int -> [Int]
primeFactors n 
  = primeFactors' n 2
  where 
    primeFactors' :: Int -> Int -> [Int] 
    primeFactors' n x
      | x >= n         = [] 
      | n `mod` x == 0 = x : (primeFactors' (n `div` x) x)
      | otherwise      = primeFactors' n (x+1)

hcf :: Int -> Int -> Int
hcf n m 
  = foldl1 (*) xs
  where 
    xs    = nList \\ (nList \\ mList)
    nList = primeFactors n 
    mList = primeFactors m

lcm' :: Int -> Int -> Int
lcm' a b
  = product (fs' \\ fs) * min a b
  where
   fs = primeFactors (min a b)
   fs' = primeFactors (max a b)

findAll x t = [y | (m, y) <- t, m == x]

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x ms = [(p, q) |(p, q) <- ms, p /= x]

remove' :: Eq a => a -> [(a, b)] -> [(a, b)]
remove' x ms = filter (\(m, n) -> m /= x) ms

qsort :: [Int] -> [Int]
qsort []
  = []
qsort (x : xs)
  = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

allSplits :: [a] -> [([a], [a])]
allSplits xs = [splitAt m xs | m <- [1..((length xs) - 1)]]

prefixes :: [t] -> [[t]]
prefixes [] = []
prefixes xs = [take n xs | n <- [1..length xs]]

substrings :: String -> [String] 
substrings [] = []
substrings xs = concatMap prefixes [drop n xs | n <- [0..(length xs - 1)]]

perms :: Eq a => [a] -> [[a]]
perms xs = [ x : ps | x <- xs, ps <- perms (xs \\ [x])] 

routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes m n g 
  | m == n    = [[]]
  | otherwise = [ m : ps | m'' <- [ m'' | (m', m'') <- g, m' == m], ps <- (routes m'' n g)]

greet :: Maybe Int -> String
greet xs 
  = case xs of 
       Nothing -> "Who are you?"
       Just x
         | x > 0     -> "Hello, Mr. Positive!"
         | otherwise -> "Hello, Mr. Negative!"

depunctuate :: String -> String
depunctuate = filter (flip notElem ".,:")

makeString :: [Int] -> String 
makeString = map chr

enpower :: [Int] -> Int
enpower = foldl1 (flip (^))

enpower' :: [Int] -> Int 
enpower' [n]
  = n
enpower' (n : ns)
  = enpower' ns ^ n

revAll :: [[a]] -> [a]
revAll = concatMap reverse

rev'' :: [a] -> [a]
rev'' = foldl (flip (:)) [] 

dezip :: [(a, b)] -> ([a], [b]) 
dezip
  = foldl (\(xs, ys) (x, y) -> (x : xs, y : ys)) ([], [])

allSame :: [Int] -> Bool
allSame [] = True
allSame (x : xs) = and (map (== x) xs)

factorials :: [Float]
factorials = scanl1 (*) [1,2..]

e :: Float
e = converge (\x y -> abs (x - y) < 0.00001) (scanl (+) 1 (map (1/) factorials))

squash :: (a -> a -> b) -> [a] -> [b]
squash _ [] = [] 
squash f xs = zipWith f xs (tail xs)

converge :: (a -> a -> Bool) -> [a] -> a
converge f xs = snd (head (dropWhile (not . fst) c))
  where 
    b = zipWith f xs (tail xs)
    c = zip (b ++ [True]) xs

converge' :: (a -> a -> Bool) -> [a] -> a
converge' f [x] = x 
converge' f (x : y : xs) = if (f x y) then y else converge' f (y : xs) 

limit :: (a -> a -> Bool) -> [a] -> [a]
limit f (x : y : ys)
  | f x y = [x, y]
  | otherwise = x : limit f (y : ys)
limit f xs
  = xs

repeatUntil :: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil p f
  = head . filter p . iterate f 

any' p = or . p 
and' p = and . p  

isElem :: Eq a => a -> [a] -> Bool 
isElem = any . (==)   

infixl 9 <.>
(<.>) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(f <.> g) x y = f (g x y) 

isOne :: Int -> Bool
isOne 1 = True 
isOne _ = False 

pipeline :: [a -> a] ->[a] -> [a]
pipeline = map . foldr1 (.)

data Shape = Triangle Float Float Float | 
              Square Float |
              Circle Float |
              Polygon [(Float, Float)]

area :: Shape -> Float 
area (Triangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
  where 
    s = (a + b + c) / 2
area (Square a) = a ^ 2 
area (Circle a) = pi * a ^ 2
area (Polygon (x : y : z : vs))
  = area (Triangle a b c) + area (Polygon vs)
  where 
    a = dist x y 
    b = dist y z 
    c = dist x z 
    dist (m, n) (m', n')
      = sqrt ((m - m') ^ 2 + (n - n') ^ 2)
area (Polygon _) = 0 

type Date = (Int, Int, Int) 

age :: Date -> Date -> Int
age (d, m, y) (d', m', y')
  | (m, d) <= (m', d') = y' - y
  | otherwise          = y' - y - 1 

-- data Tree a = Empty | Node a (Tree a) (Tree a)
--               deriving(Show, Eq)

-- data Tree = Leaf | Node' Tree Tree
--           deriving (Eq, Show)

-- data Tree a = Leaf a | Node (Tree a) (Tree a)  
--               deriving (Show) 

data Tree a b = Node a (Tree a b) (Tree a b) |
                Leaf b |
                Empty 

-- flatten :: Tree a -> [a]
-- flatten t = flatten' t [] 
--   where 
--   flatten' Empty xs
--     = xs
--   flatten' (Node x t1 t2) xs 
--     = flatten' t1 (x : (flatten' t2 xs))

-- makeTrees :: Int -> [Tree]
-- makeTrees 0 = [Leaf]
-- makeTrees n = [Node' l r | t <- [0..n - 1], 
--                            t' <- [0..n - 1], 
--                            t + t' == n - 1, 
--                            l <- (makeTrees t), r <- (makeTrees t')]

-- build :: [a] -> Tree a 
-- build [a] = Leaf a
-- build xs  = Node (build t1) (build t2)   
--   where 
--     n        = (length xs) `div` 2   
--     (t1, t2) = splitAt n xs         

-- ends :: Tree a -> [a]
-- ends (Leaf a) = [a] 
-- ends (Node t1 t2) = (ends t1) ++ (ends t2) 

-- swap :: Tree a -> Tree a 
-- swap (Leaf a) = Leaf a 
-- swap (Node t1 t2) = Node (swap t2) (swap t1)

mapT :: (b -> b) -> (a -> a) -> Tree a b -> Tree a b 
mapT _ _ Empty = Empty 
mapT f _ (Leaf b) = Leaf (f b)
mapT f g (Node a t1 t2) = Node (g a) (mapT f g t1)
                                     (mapT f g t2)

foldT :: (a -> b) -> (c -> b -> b -> b) -> b -> Tree c a -> b
foldT leafFun nodeFun b Empty
  = b
foldT leafFun nodeFun b (Leaf x)
  = leafFun x
foldT leafFun nodeFun b (Node x t1 t2)
  = nodeFun x (foldInto t1) (foldInto t2)
  where
    foldInto = foldT leafFun nodeFun b

data Colour = Red | Green | Blue
              deriving (Show, Bounded, Enum)

data AmPm = AM | PM
          deriving (Enum, Show)

data Time = TwentyFour Int Int |
            WallClock Int Int AmPm

to24 :: Time -> Time
to24 (WallClock h m AM)
  | h == 12 = TwentyFour 0 m
  | otherwise = TwentyFour h m
to24 (WallClock h m PM)
  | h == 12 = TwentyFour h m
  | otherwise = TwentyFour (h + 12) m
to24 t
  = t

equalTime :: Time -> Time -> Bool
equalTime t1 t2
  = isSame (to24 t1) (to24 t2)
  where
    isSame (TwentyFour h m) (TwentyFour h' m')
      = h == h' && m == m'   

instance Eq Time where 
  (==) = equalTime  

instance Show Time where 
  show (TwentyFour a b) = show (a * 100 + b) ++ "hrs" 
  show (WallClock 12 00 AM) = "Midnight"
  show (WallClock 12 00 PM) = "Midday"
  show (WallClock h m n) = show (h * 100 + m) ++ show n

type VarName = String

data Fun = Add | Sub | Mul
          deriving (Eq, Show)

data Exp = Val Int | Id VarName | App Fun Exp Exp
          deriving (Eq, Show)

type Assignment = (VarName, Exp)

type Program = [Statement]

data Statement = A Assignment | Loop Int Program

type Environment a = [(VarName, a)]

infixl 1 <--
(<--) :: VarName -> Exp -> Statement
(<--)
  = (A .) . (,)

loop :: Int -> Program -> Statement
loop = Loop

class Vars a where
  x, y, z :: a

instance Vars Exp where 
  x = Id "x"
  y = Id "y"
  z = Id "z" 

instance Vars String where 
  x = "x"
  y = "y"
  z = "z"

instance Num Exp where 
  (+) = App Add 
  (*) = App Mul
  (-) = App Sub 
  fromInteger = Val . fromInteger 
  signum = undefined 
  abs = undefined

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
  = (fromJust .) . lookup

update :: VarName -> Int -> Environment Int -> Environment Int
update v x env
  = (v, x) : [p | p@(v', x') <- env, v /= v']

eval :: Exp -> Environment Int -> Int
eval (Val n) env
  = n
eval (Id id) env
  = lookUp id env
eval (App f e e') env
  = apply f (eval e env) (eval e' env)

apply op v v'
  = lookUp op [(Add, (+)),(Sub, (-)), (Mul, (*))] v v'

evalS :: Statement -> Environment Int -> Environment Int
evalS (A (v, e)) env
  = update v (eval e env) env
evalS (Loop n p) env
  = foldr run' env (replicate n p)

run :: Program -> Environment Int
run p
  = run' p []

run' p env
  = foldr evalS env (reverse p)

p1 :: Program
p1 = [x <-- 8,
      y <-- 0,
      z <-- 11,
      loop 2 [x <-- x + z - y,
              z <-- x - y + z,
              y <-- 2 * y - x]]


  















 






