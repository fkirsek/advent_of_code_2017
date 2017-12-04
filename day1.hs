import Data.Char
import Data.Array

sumDuplicates :: [Int] -> Int
sumDuplicates []         = 0
sumDuplicates (x:y:rest) 
    | x == y    = x + sumDuplicates (y:rest)
    | otherwise = sumDuplicates (y:rest)
sumDuplicates _	= 0

sumCircularDuplicates :: [Int] -> Int
sumCircularDuplicates [x] = 0
sumCircularDuplicates (x:rest)
    | x == last rest = x + sumDuplicates (x:rest)
    | otherwise = sumDuplicates (x:rest)
sumCircularDuplicates list = sumDuplicates list 

solution :: [Char] -> Int
solution x = sumCircularDuplicates . map digitToInt $ x

-- b part of day1

sumHalwayList :: [Int] -> Int
sumHalwayList l = sum zipped
    where   zipped = zipWith (\x y -> if x == y then x + y else 0 ) h1 h2
            (h1,h2) = splitAt (length l `div` 2) l 

solutionB :: [Char] -> Int
solutionB x = sumHalwayList . map digitToInt $ x