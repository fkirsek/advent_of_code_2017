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

filterFirstMatching :: [Int] -> Int -> [Int]
filterFirstMatching [] _     = []
filterFirstMatching (x:xs) y
     | x == y = xs
     | otherwise = x : filterFirstMatching xs y

listIntoPairList :: [Int] -> [(Int, [Int])]
listIntoPairList l = map (\x -> (x, filterFirstMatching l x)) l

--should be done with Maybe
findDivisor :: (Int, [Int]) -> Int
findDivisor (_, []) = 0
findDivisor (x, (y:ys))
     | x `mod` y == 0 = x `div`y
     | otherwise = findDivisor (x, ys)

listPairList ll     = map listIntoPairList ll
listListDivisors ll = map (map findDivisor) (listPairList ll)

solutionBList :: [[Int]] -> Int
solutionBList ll =  sum $ map sum (listListDivisors ll)
	
solutionB :: [Char] -> Int
solutionB s = solutionBList $ map (map read) $ map words (lines s)
