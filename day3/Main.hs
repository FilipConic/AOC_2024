import FileReading
import Data.Char(isDigit, digitToInt)
import Data.Maybe (isJust, fromJust)

findInStr::String -> String -> Bool
findInStr [] [] = True
findInStr [] (y:ys) = True
findInStr (x:xs) [] = False
findInStr (x:xs) (y:ys)
    | x == y = findInStr xs ys
    | otherwise = False

findTwoNums::String -> Maybe (Int, Int, String)
findTwoNums xs =
    let (num1, str1) = head (reads xs::[(Int, String)]) in
        if head str1 == ',' then
            let (num2, str2) = head (reads (tail str1)::[(Int, String)]) in
                if head str2 == ')' then Just (num1, num2, tail str2)
                else Nothing
        else Nothing

findMults::String -> [(Int, Int)]
findMults xs = holding xs "" True
    where
        holding::String -> String -> Bool -> [(Int, Int)]
        holding [] _ _ = []
        holding (x:xs) ys next
            | findInStr ys "mul(" = 
                if ys == "mul(" then
                    let ret = findTwoNums (x:xs) in
                    if isJust ret && next then
                        let (num1, num2, str) = fromJust ret in
                            (num1, num2) : holding str "" next
                    else holding xs "" next
                else holding xs (ys ++ [x]) next
            | findInStr ys "do()" =
                if ys == "do()" then holding (x:xs) "" True
                else holding xs (ys ++ [x]) next
            | findInStr ys "don\'t()" =
                if ys == "don\'t()" then holding (x:xs) "" False
                else holding xs (ys ++ [x]) next
            | otherwise = holding (x:xs) "" next

main::IO()
main = do
    file <- setupFile ".\\day3\\input.txt"
    print $ sum $ map (\(x, y) -> x * y) $ findMults file