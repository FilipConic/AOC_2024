import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char (digitToInt, isDigit)

findMax::[Int] -> Int
findMax = foldr max (- 1000000)

findMin::[Int] -> Int
findMin = foldr min 1000000

removeFromList::[Int] -> Int -> [Int]
removeFromList (x:xs) y
    | x == y = xs
    | otherwise = x : removeFromList xs y

sortList::[Int] -> [Int]
sortList [x] = [x]
sortList xs = foundMin : sortList (removeFromList xs foundMin)
    where
        foundMin::Int
        foundMin = findMin xs

seperate:: String -> [String]
seperate = foldl (\acc x ->
            if x /= ' ' && x /= '\n' && x /= ',' && x /= '.'
                then (head acc ++ [x]) : tail acc
            else
                if null $ head acc then acc
                else [] : acc) [[]]


stringsToInts:: [String] -> [Int]
stringsToInts = map $ foldl (\acc x -> acc * 10 + digitToInt x) 0

firstList:: [Int] -> [Int]
firstList [] = []
firstList (x:y:xs) = x : firstList xs

secondList:: [Int] -> [Int]
secondList [] = []
secondList (x:y:xs) = y : secondList xs

countIn:: [Int] -> Int -> Int
countIn [] _ = 0
countIn (x:xs) y 
    | x == y = 1 + countIn xs y
    | otherwise = countIn xs y

compLists:: [Int] -> [Int] -> [Int]
compLists [] _ = []
compLists (x:xs) ys = (x * countIn ys x) : compLists xs ys

calculate:: String -> Int
calculate xs = sum $ compLists first second
    where
        list = stringsToInts $ tail $ seperate xs
        (first, second) = (sortList $ firstList list, sortList $ secondList list)
        

main::IO()
main = do
    hSetBuffering stdout NoBuffering
    file <- readFile ".\\day1\\input.txt"
    print $ calculate file