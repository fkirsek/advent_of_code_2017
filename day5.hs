import Data.Array
import Control.Monad.ST
import Data.Array.ST

traverseAndChange :: Array Int Int -> Int -> Int -> Int
traverseAndChange arr currentInd totalSteps
    | currentInd > lastIndex = totalSteps
    | otherwise = traverseAndChange (arr // [(currentInd, currVal + 1)] ) nextInd (totalSteps+1)
        where currVal = arr ! currentInd
              nextInd = currVal + currentInd
              (_, lastIndex) = bounds arr

solutionA :: [Char] -> Int
solutionA s = traverseAndChange arr 0 0
	where l = map read $ words s
	      arr = listArray (0, length l - 1) l

traverseAndChangeB :: Array Int Int -> Int -> Int -> Int
traverseAndChangeB arr currentInd totalSteps
    | currentInd > lastIndex = totalSteps
    | otherwise = traverseAndChangeB (arr // [(currentInd, currVal + strangeIncrease)] ) nextInd (totalSteps+1)
        where currVal = arr ! currentInd
              nextInd = currVal + currentInd
              (_, lastIndex) = bounds arr
              strangeIncrease = if currVal >= 3 then -1 else 1

solutionB :: [Char] -> Int
solutionB s = traverseAndChangeB arr 0 0
	where l = map read $ words s
	      arr = listArray (0, length l - 1) l

 traverseAndChangeState arr currentInd totalSteps
          <- do 
            (_, lastIndex)  <- getBounds arr
            if currentInd > lastIndex
            then 
              do 
                return totalSteps
            else 
              do 
              	totalSteps <- totalSteps + 1
                currVal <- readArray arr currentInd
                nextInd <- currVal + currentInd
                strangeIncrease <- if currVal >= 3 then -1 else 1
                arr <- writeArray currentInd (currVal + strangeIncrease)
                return traverseAndChangeState arr nextInd totalSteps

