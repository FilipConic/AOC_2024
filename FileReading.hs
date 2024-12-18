module FileReading where
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char(digitToInt)

setupFile:: String -> IO String
setupFile filePath = do
    hSetBuffering stdout NoBuffering
    readFile filePath

seperate:: String -> [String]
seperate = foldl (\acc x ->
            if x /= ' ' && x /= '\n' && x /= ',' && x /= '.'
                then (head acc ++ [x]) : tail acc
            else
                if null $ head acc then acc
                else [] : acc) [[]]


stringsToInts:: [String] -> [Int]
stringsToInts = map $ foldl (\acc x -> acc * 10 + digitToInt x) 0

seperateLines:: String -> [String]
seperateLines = foldl (\acc x ->
            if x /= '\n'
                then (head acc ++ [x]) : tail acc
            else
                if null $ head acc then acc
                else [] : acc) [[]]

seperateLinesToWords:: [String] -> [[String]]
seperateLinesToWords [x] = [seperate x]
seperateLinesToWords (x:xs) = seperate x : seperateLinesToWords xs

linesOfNumbers:: [[String]] -> [[Int]]
linesOfNumbers [x] = [stringsToInts x]
linesOfNumbers (x:xs) = stringsToInts x : linesOfNumbers xs
