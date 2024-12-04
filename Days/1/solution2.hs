import Data.List


splitEO :: [a] -> ([a], [a])
splitEO [] = ([], [])
splitEO [x] = ([x], [])
splitEO (x:y:xs) = (x:odds, y:evens)
    where (odds, evens) = splitEO xs

sortLists ::  Ord a => ([a], [a]) -> ([a], [a])
sortLists (xs, ys) = (sort xs, sort ys)

calcDiffs :: Num a => ([a], [a]) -> [a]
calcDiffs (xs, ys) =
    map abs $ zipWith (-) xs ys 

simScore :: [Int] -> Int -> Int
simScore xs x =
    x * length (filter (x==) xs)

totSimScore (xs, ys) =
    sum (map (simScore ys) xs)

main = 
    print . totSimScore . sortLists . splitEO .  map readInt . words =<< readFile "input.txt"


readInt :: String -> Int
readInt = read