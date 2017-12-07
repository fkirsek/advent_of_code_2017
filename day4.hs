import Data.List
isValidPassphrase :: [Char] -> Bool
isValidPassphrase s = hasDuplicates $ words s

hasDuplicates :: [[Char]] -> Bool
hasDuplicates list = hasDuplicatesWrapper list []

hasDuplicatesWrapper :: [[Char]] -> [[Char]] -> Bool
hasDuplicatesWrapper (x:xs) previous 
    | elem x previous = False
    | otherwise = hasDuplicatesWrapper xs (x:previous)
hasDuplicatesWrapper [] _ = True

solutionA :: [Char] -> Int
solutionA s = length $ filter isValidPassphrase matrix
	where matrix = lines s

isValidPassphraseB :: [Char] -> Bool
isValidPassphraseB s = hasDuplicates list
	where list = map sort $ words s

solutionB :: [Char] -> Int
solutionB s = length $ filter isValidPassphraseB matrix
	where matrix = lines s
