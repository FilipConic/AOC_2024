import FileReading
import Data.Ord (clamp)

data XMAS = X | M | A | S | Null
data ListXMAS = ListXMAS [[XMAS]] Int Int

instance Show XMAS where
    show :: XMAS -> String
    show x = case x of
        X -> "X"
        M -> "M"
        A -> "A"
        S -> "S"
        Null -> "."
instance Show ListXMAS where
    show :: ListXMAS -> String
    show (ListXMAS xmas w h) = show xmas ++ ", " ++ show w ++ ", " ++ show h

instance Eq XMAS where
    (==)::XMAS -> XMAS -> Bool
    (==) X X = True
    (==) M M = True
    (==) A A = True
    (==) S S = True
    (==) _ _ = False
    (/=)::XMAS -> XMAS -> Bool
    (/=) X X = False
    (/=) M M = False
    (/=) A A = False
    (/=) S S = False
    (/=) _ _ = True

turnToXMAS::[String] -> [[XMAS]]
turnToXMAS [] = []
turnToXMAS (x:xs) = oneLine x : turnToXMAS xs
    where
        oneLine::String -> [XMAS]
        oneLine [] = []
        oneLine (x:xs) = toXMAS x : oneLine xs
            where 
                toXMAS::Char -> XMAS
                toXMAS n = case n of
                    'X' -> X
                    'M' -> M
                    'A' -> A
                    'S' -> S
                    '.' -> Null

generateXMAS::[[XMAS]] -> ListXMAS
generateXMAS xmas = ListXMAS xmas (length xmas) (length $ head xmas) 

getElement::ListXMAS -> Int -> Int -> XMAS
getElement (ListXMAS xmas w h) x y = 
    helper (helper xmas $ clamp (0, h - 1) y) $ clamp (0, w - 1) x
    where
        helper::[a] -> Int -> a
        helper (y:_) 0 = y
        helper (y:ys) z = helper ys (z - 1)

hasXMAS::ListXMAS -> Int -> Int -> Int -> Int
hasXMAS (ListXMAS xmas w h) 0 x y = 
    -- +x
    if x >= 1 && y >= 1 && x < w - 1 && y < h - 1 &&
        getElement fullXmas x y == A && 
        getElement fullXmas (x + 1) (y + 1) == M &&
        getElement fullXmas (x + 1) (y - 1) == M &&
        getElement fullXmas (x - 1) (y + 1) == S && 
        getElement fullXmas (x - 1) (y - 1) == S then 1 else 0
    where fullXmas = ListXMAS xmas w h
hasXMAS (ListXMAS xmas w h) counter x y =
    hasXMAS fullXmas (counter - 1) x y +
    if getElement fullXmas x y == A && x >= 1 && y >= 1 && x < w - 1 && y < h - 1 &&
        case counter of
            -- +y
            1 ->getElement fullXmas (x + 1) (y + 1) == M &&
                getElement fullXmas (x + 1) (y - 1) == S &&
                getElement fullXmas (x - 1) (y + 1) == M && 
                getElement fullXmas (x - 1) (y - 1) == S
            -- -x
            2 ->getElement fullXmas (x + 1) (y + 1) == S &&
                getElement fullXmas (x + 1) (y - 1) == S &&
                getElement fullXmas (x - 1) (y + 1) == M && 
                getElement fullXmas (x - 1) (y - 1) == M
            -- -y
            3 ->getElement fullXmas (x + 1) (y + 1) == S &&
                getElement fullXmas (x + 1) (y - 1) == M &&
                getElement fullXmas (x - 1) (y + 1) == S && 
                getElement fullXmas (x - 1) (y - 1) == M
    then 1 else 0
    where fullXmas = ListXMAS xmas w h

searchForXMAS::ListXMAS -> Int -> Int -> Int
searchForXMAS (ListXMAS xmas w h) a b
    | b == h = 0
    | a < w = hasXMAS wholeXMAS 3 a b + searchForXMAS wholeXMAS (a + 1) b
    | b < h = searchForXMAS wholeXMAS 0 (b + 1)
    | otherwise = 0
    where
        wholeXMAS = ListXMAS xmas w h

listAllElements::ListXMAS -> Int -> Int -> String
listAllElements (ListXMAS xmas w h) x y
    | y == h = ""
    | x < w = 
        if hasXMAS fullXMAS 3 x y > 0 then show (hasXMAS fullXMAS 3 x y) ++ listAllElements fullXMAS (x + 1) y
        else "." ++ listAllElements fullXMAS (x + 1) y
    | y < h = "\n" ++ listAllElements fullXMAS 0 (y + 1)
    | otherwise = ""
    where
        fullXMAS = ListXMAS xmas w h

main::IO()
main = do
    file <- setupFile ".\\day4\\input.txt"
    --putStrLn $ listAllElements (generateXMAS $ reverse $ turnToXMAS $ seperateLines file) 0 0
    print $ searchForXMAS (generateXMAS $ reverse $ turnToXMAS $ seperateLines file) 0 0