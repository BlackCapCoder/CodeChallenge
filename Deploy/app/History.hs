{-# LANGUAGE LambdaCase, MultiWayIf #-}
module History where

import Data.Digits
import Math.Combinatorics.Exact.Binomial
import Math.Combinatorics.Exact.Factorial
-- import Debug.Trace


-- The user can either skip or beat a level
data Res = Skip | Beat deriving (Show, Eq)

-- A levels history are the beats/skips for all levels
-- leading up to this one
type History = [Res]


-- Infinite list of all possible past skip/beat histories for each level
histories :: Int -> [[History]]
histories max
  = mempty
  : iterate (\x -> map (++pure Beat) x
                ++ map (++pure Skip) (filter ((<max-1).length.filter(==Skip)) x)
            ) [ pure Beat, pure Skip ]

-- This has a saner order
histories' :: Int -> [History]
histories' max
  = [ map (\case 0 -> Beat; _ -> Skip) ds
    | l <- [1..]
    , ds <- pure $ tail $ digits 2 l
    , length (filter (==1) ds) < max
    ]

histories'' :: Int -> [[History]]
histories'' max
  = [ [ map (\case 0 -> Beat; _ -> Skip) ds
      | l <- [2^i..2^(i+1)-1]
      , ds <- pure $ tail $ digits 2 l
      , length (filter (==1) ds) < max]
    | i <- [0..]
    ]

-- Fast way to calculate `length $ histories 3 !! n`, which is `1 + sum [1..n]`

histLength2 0 = 0
histLength2 n = 1 + n

histLength3 0 = 0
histLength3 n = 1 + div (n^2 + n) 2

histLength4 0 = 0
histLength4 n = 1 + div (n^3 + 5*n) 6

histLength5 0 = 0
histLength5 n = 1 + div (n^4 - 6*n^3 + 23*n^2 - 18*n) 24

-- histLength6 0 = 0
histLength6 n = 1 + div (n^5 - 5*n^4 + 25*n^3 + 5*n^2 + 94*n) 120

histLength  m n = sum $ map (choose n) [0..m-1]
histLength' m n = length $ histories m !! n

histLength'' _ 0 = 0
histLength'' m n
  | x <- m-1
  = 1 + div (n^x + 0) (factorial x)


-- Cummulative history length given level. `sum $ map histLength3 [0..n]`. A003600
histLength2Cum n = histLength3  (n+1) - 2
histLength3Cum n = histLength4  (n+1) - 2
histLength4Cum n = histLength5  (n+2) - 2
histLength5Cum n = histLength6  (n+0) - 1
histLength6Cum n = histLength 7 (n+1) - 1


-- level given index
index2Level y = length . takeWhile (y>) . scanl1 (+) $ map histLength3 [0..]

-- Get the index of a given history. `length . takeWhile (/=h) $ histories' 3`
histToIndex :: History -> Int
histToIndex [] = 0
histToIndex h
  | l <- length h
  = histLength3Cum (l-1) + 1
  + if | (a,_:b) <- break (==Skip) h
       -> choose (l+1-length a) 2
       -  if | (c,_:_) <- break (==Skip) b -> length c
             | otherwise                  -> length b
       | otherwise -> 0

------

showHist [] = []
showHist (Skip:x) = 'S' : showHist x
showHist (Beat:x) = 'B' : showHist x

bino = choose
