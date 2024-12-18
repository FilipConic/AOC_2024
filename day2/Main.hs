import FileReading

isSafeAsc:: [Int] -> Bool
isSafeAsc [x] = True
isSafeAsc (x:xs) = y - x >= 1 && y - x <= 3 && isSafeAsc xs
    where
        y = head xs

isSafeDesc:: [Int] -> Bool
isSafeDesc [x] = True
isSafeDesc (x:xs) = x - y >= 1 && x - y <= 3 && isSafeDesc xs
    where
        y = head xs

removeElement:: [Int] -> Int -> [Int]
removeElement xs a = removeStart xs a 0
    where
        removeStart:: [Int] -> Int -> Int -> [Int]
        removeStart [] _ _ = []
        removeStart (x:xs) a b
            | a == b = removeStart xs a (b + 1)
            | otherwise = x : removeStart xs a (b + 1)

checkWithRemovedElement:: [Int] -> Int
checkWithRemovedElement xs =
    if checkInAllCases xs 0 $ length xs then 1 else 0
    where
        checkInAllCases:: [Int] -> Int -> Int -> Bool
        checkInAllCases xs a b
            | a /= b = (isSafeAsc list || isSafeDesc list) || checkInAllCases xs (a + 1) b
            | otherwise = False 
            where
                list = removeElement xs a

allLists:: [[Int]] -> Int
allLists [x]
    | isSafeDesc x = 1
    | isSafeAsc x = 1
    | otherwise = checkWithRemovedElement x
allLists (x:xs)
    | isSafeDesc x = 1 + allLists xs
    | isSafeAsc x = 1 + allLists xs
    | otherwise = checkWithRemovedElement x + allLists xs 

main::IO()
main = do
    file <- setupFile ".\\day2\\input.txt"
    print $ allLists $ linesOfNumbers $ seperateLinesToWords $ seperateLines file