import Data.Char

greatestDiff :: [Int] -> Int
greatestDiff (x:xs) = greatestDiffAcc xs x x
greatestDiff _      = 0

greatestDiffAcc :: [Int] -> Int -> Int -> Int
greatestDiffAcc [] smallest greatest = greatest - smallest
greatestDiffAcc (x:xs) smallest greatest
    | x < smallest && x > greatest = greatestDiffAcc xs x x
    | x < smallest = greatestDiffAcc xs x greatest
    | x > greatest = greatestDiffAcc xs smallest x
    | otherwise    = greatestDiffAcc xs smallest greatest

solutionALists :: [[Int]] -> Int
solutionALists ll  = sum $ map greatestDiff ll

solutionA :: [Char] -> Int
solutionA s = solutionALists $ map (map read) $ map words (lines s)

-- 5 1 9 5
-- 7 5 3
-- 2 4 6 8

